(ns inferenceql.query.generative-table
  (:require [inferenceql.inference.gpm.proto :as gpm.proto]
            [inferenceql.query.relation :as relation]))

(defrecord GenerativeTable [relation normalizing-constant]
  gpm.proto/GPM
  (simulate [_ targets _constraints]
    (loop [remainder (rand normalizing-constant)
           tuples (relation/tuples relation)]
      (let [{:syms [probability] :as tuple} (first tuples)]
        (if (< remainder probability)
          (select-keys tuple (map symbol targets))
          (recur (- remainder probability)
                 (rest tuples))))))

  gpm.proto/Variables
  (variables [_]
    (-> (relation/attributes relation)
        (set)
        (disj 'probability))))

(defn generative-table [relation]
  (let [normalizing-constant (transduce (map 'probability)
                                        +
                                        (relation/tuples relation))]
    (->GenerativeTable relation normalizing-constant)))
