(ns inferenceql.query.permissive
  "Functions for issuing IQL-permissive queries."
  (:require #?(:clj [clojure.core.match :as match]
               :cljs [cljs.core.match :as match])
            [clojure.walk :as walk]
            [inferenceql.query.base :as base]
            [inferenceql.query.string :as string]
            [inferenceql.query.sequence :as sequence]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.permissive.parser :as parser]))

(def ^:private ws [:ws " "])

(defn model-expr
  "Takes a model node and an event node and produces a new model-expr node."
  [model event]
  (case (tree/tag event)
    :given-event-and
    (let [events (tree/child-nodes event)]
      (reduce model-expr model events))

    :density-event-eq
    [:conditioned-by-expr model ws "CONDITIONED" ws "BY" ws event]

    :distribution-event-binop
    [:constrained-by-expr model ws "CONSTRAINED" ws "BY" ws event]))

(defn strict-node
  [node]
  (if-not (tree/branch? node)
    node
    (match/match (vec (remove tree/whitespace? node))
      [:probability-expr prob of [:permissive-density & events] under model]
      (let [density (string/match-case "density" prob)
            separator [ws  (string/match-case "and" prob) ws]
            events (->> events (filter tree/branch?) (remove tree/whitespace?))
            event (if (> (count events) 1)
                    `[~:density-event-and ~@(sequence/intersperse events separator)]
                    (first events))]
        [:density-expr prob ws density ws of ws event ws under ws model])

      [:probability-expr prob of [:permissive-distribution & events] under model]
      (let [separator [ws  (string/match-case "and" prob) ws]
            events (->> events (filter tree/branch?) (remove tree/whitespace?))
            event (if (> (count events) 1)
                    `[~:distribution-event-and ~@(sequence/intersperse events separator)]
                    (first events))]
        [:probability-expr prob ws of ws event ws under ws model])

      [:given-expr & children]
      (let [child-nodes (filter tree/branch? children)
            model (first child-nodes)
            event (last child-nodes)]
        (model-expr model event))

      :else node)))

(defn ->strict
  [node]
  (walk/postwalk strict-node node))

(defn parse
  [s]
  (-> s (parser/parse) (->strict)))

(defn query
  "Issues a query against a database. Returns a relation or nil."
  [s db]
  (base/query s db parse))

(defn q
  "Returns the result of executing a query on a set of rows."
  [s db]
  (base/q s db parser/parse))
