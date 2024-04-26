(ns gensql.query.generative-table
  (:require [clojure.math :as math]
            [gensql.inference.gpm.constrained :as constrained]
            [gensql.inference.gpm :as gpm]
            [gensql.inference.gpm.proto :as gpm.proto]
            [gensql.query.relation :as relation]))

(def constraints->pred @#'constrained/event->pred) ; accessing private function

(declare generative-table)

(defn conditions->pred
  [event]
  (fn [m]
    (let [event (update-keys event symbol)]
      (= event (select-keys m (keys event))))))

(defn normalizing-constant
  [prob-fn coll]
  (transduce (map prob-fn)
             +
             coll))

(defn weighted-sample
  [prob-fn coll]
  (loop [rem (rand (normalizing-constant prob-fn coll))
         coll coll]
    (let [curr (first coll)
          prob (prob-fn curr)]
      (if (<= rem prob)
        curr
        (recur (- rem prob)
               (next coll))))))

(defrecord GenerativeTable [relation]
  gpm.proto/GPM
  (simulate [_ targets conditions]
    (let [satisfies-conditions? (conditions->pred conditions)
          relation (if-not (seq conditions)
                     relation
                     (relation/select relation satisfies-conditions?))
          tuple (weighted-sample 'probability (relation/tuples relation))]
      (select-keys tuple (map symbol targets))))

  (logpdf [this targets conditions]
    (if (seq conditions)
      (let [conditioned (gpm/condition this conditions)]
        (gpm/logpdf conditioned targets {}))
      (let [pred? (conditions->pred targets)
            tuples (relation/tuples relation)
            log-likelihood (-> (transduce (comp (filter pred?)
                                                (map 'probability))
                                          +
                                          tuples)
                               (math/log))
            log-normalizing-constant (-> (normalizing-constant 'probability tuples)
                                         (math/log))]
        (- log-likelihood log-normalizing-constant))))

  gpm.proto/Condition
  (condition [_ conditions]
    (let [satisfies-conditions? (conditions->pred conditions)]
      (generative-table (relation/select relation satisfies-conditions?))))

  gpm.proto/Constrain
  (constrain [_ event opts]
    (let [pred (constraints->pred event opts)]
      (generative-table (relation/select relation pred))))

  gpm.proto/Variables
  (variables [_]
    (-> (relation/attributes relation)
        (set)
        (disj 'probability))))

(defn generative-table [relation]
  (->GenerativeTable relation))
