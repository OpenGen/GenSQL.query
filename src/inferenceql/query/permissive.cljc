(ns inferenceql.query.permissive
  "Functions for issuing IQL-permissive queries."
  (:require [clojure.walk :as walk]
            [inferenceql.query.base :as base]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.permissive.parser :as parser]))

(def ^:private ws [:ws " "])

(defn ^:private conditioned-by
  [model event]
  [:conditioned-by-expr model ws "CONDITIONED" ws "BY" ws event])

(defn ^:private constrained-by
  [model event]
  [:constrained-by-expr model ws "CONSTRAINED" ws "BY" ws event])

(defn transform-event
  [model event]
  (case (tree/tag event)
    :given-event-and
    (let [events (tree/child-nodes event)]
      (reduce transform-event model events))

    :density-event-eq
    (conditioned-by model event)

    :distribution-event-binop
    (constrained-by model event)))

(defn given->strict
  [node]
  (if (and (tree/branch? node)
           (= :given-expr (tree/tag node)))
    (let [children (tree/child-nodes node)
          model (first children)
          event (last children)]
      (transform-event model event))
    node))

(defn ->strict
  [node]
  (walk/postwalk given->strict node))

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
