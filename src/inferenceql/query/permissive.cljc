(ns inferenceql.query.permissive
  "Functions for issuing IQL-permissive queries."
  (:require #?(:clj [clojure.core.match :as match]
               :cljs [cljs.core.match :as match])
            [clojure.string :as string]
            [clojure.walk :as walk]
            [inferenceql.query.base :as base]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.permissive.parser :as parser]
            [inferenceql.query.sequence :as sequence]
            [inferenceql.query.string :as query.string]))

(def ^:private ws [:ws " "])

(defn density-event?
  [x]
  (if-let [tag (tree/tag x)]
    (-> tag
        (name)
        (string/starts-with? "density-event"))
    false))

(defn distribution-event?
  [x]
  (if-let [tag (tree/tag x)]
    (-> tag
        (name)
        (string/starts-with? "distribution-event"))
    false))

(defn model-expr
  "Takes a model node and an event node and produces a new model-expr node."
  [model event]
  (cond (density-event? event)
        [:conditioned-by-expr [:model-expr model] ws "CONDITIONED" ws "BY" ws [:density-event event]]

        (distribution-event? event)
        [:constrained-by-expr [:model-expr model] ws "CONSTRAINED" ws "BY" ws [:distribution-event event]]))

(defn variable-node
  [node]
  (case (tree/tag node)
    :simple-symbol [:variable "VAR" ws node]))

(defn strict-node
  [node]
  (match/match [(vec (remove tree/whitespace? node))]
    [[:density-event-eq ([:simple-symbol _] :as sym) equals scalar-expr]]
    [:density-event-eq (variable-node sym) ws equals ws [:scalar-expr scalar-expr]]

    [[:density-event-eq scalar-expr equals ([:simple-symbol _] :as sym)]]
    [:density-event-eq [:scalar-expr scalar-expr] ws equals ws (variable-node sym)]

    [[:distribution-event-binop ([:simple-symbol _] :as sym) binop scalar-expr]]
    [:distribution-event-binop (variable-node sym) ws binop ws [:scalar-expr scalar-expr]]

    [[:distribution-event-binop scalar-expr binop ([:simple-symbol _] :as sym)]]
    [:distribution-event-binop [:scalar-expr scalar-expr] ws binop ws (variable-node sym)]

    [[:generate-expr generate [:simple-symbol-list & children] under model]]
    (let [variable-list (into [:variable-list]
                              (map (fn [child]
                                     (if (= :simple-symbol (tree/tag child))
                                       (variable-node child)
                                       child)))
                              children)]
      [:generate-expr generate ws variable-list ws under ws model])

    [[:density-event-list & events]]
    (let [separator [ws "AND" ws]
          events (->> events
                      (filter tree/branch?)
                      (remove tree/whitespace?)
                      (map tree/only-child))]
      (if (> (count events) 1)
        `[~:density-event-and ~@(sequence/intersperse events separator)]
        (first events)))

    [[:distribution-event-list & events]]
    (let [separator [ws "AND" ws]
          events (->> events
                      (filter tree/branch?)
                      (remove tree/whitespace?)
                      (map tree/only-child))]
      (if (> (count events) 1)
        `[~:distribution-event-and ~@(sequence/intersperse events separator)]
        (first events)))

    [[:probability-expr prob of (event :guard density-event?) under model]]
    (let [density (query.string/match-case "density" prob)]
      [:density-expr prob ws density ws of ws [:density-event event] ws under ws model])

    [[:probability-expr prob of (event :guard distribution-event?) under model]]
    [:probability-expr prob ws of ws [:distribution-event event] ws under ws model]

    [[:given-expr [:model-expr model] _given event-list]]
    (let [events (tree/child-nodes event-list)]
      (reduce model-expr model events))

    :else node))

(defn ->strict
  [node]
  (let [f (fn [x]
            (if (tree/branch? x)
              (strict-node x)
              x))]
    (walk/postwalk f node)))

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
