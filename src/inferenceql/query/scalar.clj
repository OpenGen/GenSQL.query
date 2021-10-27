(ns inferenceql.query.scalar
  (:refer-clojure :exclude [eval])
  (:require [clojure.core.match :as match]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]
            [hashp.core]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.math :as math]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.tuple :as tuple]
            [medley.core :as medley]
            [sci.core :as sci]))

(s/def ::plan
  (s/or :scalar (s/or :number number? :string string? :keyword keyword? :symbol symbol?)
        :map (s/map-of keyword? ::plan)
        :vector (s/and vector? (s/coll-of ::plan))
        :function-application (s/and (some-fn seq? list?) (s/cat :function symbol? :args (s/* ::plan)))))

(defn plan
  [node]
  (match/match (into (empty node)
                     (remove tree/whitespace?)
                     node)
    [:scalar-expr child]           (plan child)
    [:scalar-expr-group _ child _] (plan child)

    [:expr-not _not child] `(~'not ~(plan child))

    [:expr-disjunction    left _ right] `(~'or  ~(plan left) ~(plan right))
    [:expr-conjunction    left _ right] `(~'and ~(plan left) ~(plan right))
    [:expr-addition       left _ right] `(~'+   ~(plan left) ~(plan right))
    [:expr-addition       left _ right] `(~'+   ~(plan left) ~(plan right))
    [:expr-subtraction    left _ right] `(~'-   ~(plan left) ~(plan right))
    [:expr-multiplication left _ right] `(~'*   ~(plan left) ~(plan right))
    [:expr-division       left _ right] `(~'/   ~(plan left) ~(plan right))

    [:expr-binop left [:binop [:is _]]       right] `(~'=         ~(plan left) ~(plan right))
    [:expr-binop left [:binop [:is-not & _]] right] `(~'not=      ~(plan left) ~(plan right))
    [:expr-binop left [:binop s]             right] `(~(symbol s) ~(plan left) ~(plan right))

    [:distribution-event child] (plan child)

    [:distribution-event-or  left _or  right] [:or  (plan left) (plan right)]
    [:distribution-event-and left _and right] [:and (plan left) (plan right)]

    [:distribution-event-binop (variable :guard (tree/tag-pred :variable))    [:binop s] (scalar   :guard (tree/tag-pred :scalar-expr))] [(keyword s) (plan variable) (plan scalar)]
    [:distribution-event-binop (scalar   :guard (tree/tag-pred :scalar-expr)) [:binop s] (variable :guard (tree/tag-pred :variable))]    [(keyword s) (plan variable) (plan scalar)]

    [:distribution-event-group child] (plan child)

    [:density-event child] (plan child)
    [:density-event-and & children] (into {} (comp (filter tree/branch?) (map plan)) children)

    [:density-event-eq (variable :guard (tree/tag-pred :variable))    _= (scalar   :guard (tree/tag-pred :scalar-expr))] {(plan variable) (plan scalar)}
    [:density-event-eq (scalar   :guard (tree/tag-pred :scalar-expr)) _= (variable :guard (tree/tag-pred :variable))]    {(plan variable) (plan scalar)}

    [:probability-expr _probability          _of event _under model] `(~'iql/prob ~(plan model) ~(plan event))
    [:density-expr     _probability _density _of event _under model] `(~'iql/pdf  ~(plan model) ~(plan event))

    [:model-expr child] (plan child)
    [:conditioned-by-expr model _conditioned _by event] `(~'iql/condition ~(plan model) ~(plan event))
    [:constrained-by-expr model _constrained _by event] `(~'iql/constrain ~(plan model) ~(plan event))

    [:value [:null _]] nil
    [:value [_ child]] (edn/read-string child)

    [:variable _var child] (keyword (plan child))

    [:simple-symbol s] (symbol s)))

(defn inference-event
  [event]
  (walk/postwalk (fn [x]
                   (cond (vector? x) (seq x)
                         (keyword? x) (symbol x)
                         :else x))
                 event))

(defn prob
  [model event]
  (let [event (inference-event event)]
    (math/exp (gpm/logprob model event))))

(defn pdf
  [model event]
  (let [event (medley/map-keys keyword event)]
    (math/exp (gpm/logpdf model event {}))))

(defn condition
  [model conditions]
  (let [conditions (medley/map-keys keyword conditions)]
    (cond-> model
      (every? some? (vals conditions))
      (gpm/condition conditions))))

(defn constrain
  [model event]
  (let [event (inference-event event)]
    (cond-> model
      (empty? (filter nil? (tree-seq seqable? seq event)))
      (gpm/constrain event
                     {:operation? list?
                      :operands rest
                      :operator first
                      :variable? symbol?}))))

(def namespaces
  {'inferenceql.inference.gpm {}
   'clojure.core {'not not
                  '> >
                  '>= >=
                  '= =
                  '<= <=
                  '< <
                  '+ +
                  '- -
                  '* *
                  '/ /}
   'iql {'prob prob
         'pdf pdf
         'condition condition
         'constrain constrain}})

(defn eval
  ([sexpr env]
   (eval sexpr env {}))
  ([sexpr env tuple]
   (let [bindings (merge (zipmap (tuple/attributes tuple)
                                 (repeat nil))
                         (tuple/->map tuple)
                         env)
         ;; FIXME write a function to produce this automatically
         ;; from `env`
         opts {:namespaces namespaces
               :bindings bindings}]
     (try (sci/eval-string (pr-str sexpr) opts)
          (catch clojure.lang.ExceptionInfo ex
            (if-let [[_ sym] (re-find #"Could not resolve symbol: (.+)$"
                                      (ex-message ex))]
              (let [sym (symbol sym)]
                (when-not (contains? (set (tuple/attributes tuple))
                                     sym)
                  (throw (ex-info (str "Could not resolve symbol: " (pr-str sym))
                                  {:symbol sym
                                   :env bindings}))))
              (throw ex)))))))
