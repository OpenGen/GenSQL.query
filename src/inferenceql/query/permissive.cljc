(ns inferenceql.query.permissive
  "Functions for issuing IQL-permissive queries, and converting them to strict
  queries."
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

(defn fif
  "Returns a function that calls f on its argument if calling pred? on its
  argument returns true."
  [f pred?]
  (fn fif-fn [x]
    (cond-> x
      (pred? x) (f))))

(defn density-event?
  "Returns true if x is a density event."
  [x]
  (if-let [tag (tree/tag x)]
    (string/starts-with? (name tag) "density-event")
    false))

(defn distribution-event?
  "Returns true if x is a distribution event."
  [x]
  (if-let [tag (tree/tag x)]
    (string/starts-with? (name tag) "distribution-event")
    false))

(def simple-symbol-list?
  (tree/tag-pred :simple-symbol-list))

(defn model-expr
  "Takes a model node and an event node and produces a new model-expr node."
  [model event]
  (cond (density-event? event)
        [:conditioned-by-expr [:model-expr model] ws "CONDITIONED" ws "BY" ws [:density-event event]]

        (distribution-event? event)
        [:constrained-by-expr [:model-expr model] ws "CONSTRAINED" ws "BY" ws [:distribution-event event]]

        :else
        (throw (ex-info (str "Cannot generate model expression for event")
                        {:event event}))))

(defn symbol->variable
  "Returns the variable node equivalent of a simple symbol node."
  [node]
  [:variable "VAR" ws node])

(defn symbol-list->variable-list
  [node]
  (into [:variable-list]
        (map (fif symbol->variable (tree/tag-pred :simple-symbol)))
        (rest node)))

(defn symbol->density-event-eq
  "Given a simple symbol node for returns the density event for when the
  variable of that name equals the value held by that symbol in the
  environment."
  [node]
  (if-not (= :simple-symbol (tree/tag node))
    node
    (let [variable (symbol->variable node)
          scalar-expr-node [:scalar-expr node]]
      [:density-event-eq variable ws "=" ws scalar-expr-node])))

(defn and-node
  [tag nodes]
  (let [separator [ws "AND" ws]]
    (if (> (count nodes) 1)
      `[~tag ~@(sequence/intersperse nodes separator)]
      (first nodes))))

(defn strict-node
  [node]
  (match/match [(vec (remove tree/whitespace? node))]
    [[:density-event-eq ([:simple-symbol _] :as sym) equals scalar-expr]]
    [:density-event-eq (symbol->variable sym) ws equals ws [:scalar-expr scalar-expr]]

    [[:density-event-eq scalar-expr equals ([:simple-symbol _] :as sym)]]
    [:density-event-eq [:scalar-expr scalar-expr] ws equals ws (symbol->variable sym)]

    [[:distribution-event-binop ([:simple-symbol _] :as sym) binop scalar-expr]]
    [:distribution-event-binop (symbol->variable sym) ws binop ws [:scalar-expr scalar-expr]]

    [[:distribution-event-binop scalar-expr binop ([:simple-symbol _] :as sym)]]
    [:distribution-event-binop [:scalar-expr scalar-expr] ws binop ws (symbol->variable sym)]

    [[:generate-expr generate [:simple-symbol-list & nodes] under model]]
    (let [variable-list (into [:variable-list]
                              (map (fif symbol->variable (tree/tag-pred :simple-symbol)))
                              nodes)]
      [:generate-expr generate ws variable-list ws under ws model])

    [[:density-event-list & _]]
    (and-node :density-event-and
              (map tree/only-child
                   (tree/child-nodes node)))

    [[:distribution-event-list & _]]
    (and-node :distribution-event-and
              (map tree/only-child
                   (tree/child-nodes node)))

    [[:probability-expr prob of (event :guard density-event?) under model]]
    (let [density (query.string/match-case "density" prob)]
      [:density-expr prob ws density ws of ws [:density-event event] ws under ws model])

    [[:probability-expr prob of (event :guard distribution-event?) under model]]
    [:probability-expr prob ws of ws [:distribution-event event] ws under ws model]

    [[:mutual-info-expr mutual info
      of (of-list :guard simple-symbol-list?)
      with (with-list :guard simple-symbol-list?)
      under model]]
    (let [approx (query.string/match-case "approximate" mutual)]
      [:approx-mutual-info-expr approx ws mutual ws info ws
       of ws (symbol-list->variable-list of-list) ws
       with ws (symbol-list->variable-list with-list) ws
       under ws model])

    [[:mutual-info-expr mutual info
      of (of-event :guard distribution-event?)
      with (with-event :guard distribution-event?)
      under model]]
    [:mutual-info-expr mutual ws info ws
     of ws of-event ws
     with ws with-event ws
     under ws model]

    [[:given-expr [:model-expr model] _given events]]
    (transduce (map symbol->density-event-eq)
               (completing model-expr)
               model
               (tree/child-nodes events))

    [[:standalone-event-conjunction & _]]
    (and-node :density-event-and
              (map symbol->density-event-eq
                   (tree/child-nodes node)))

    [[:standalone-event-list & _]]
    (and-node :density-event-and
              (map symbol->density-event-eq
                   (tree/child-nodes node)))

    :else
    node))

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
  (base/q s db parse))
