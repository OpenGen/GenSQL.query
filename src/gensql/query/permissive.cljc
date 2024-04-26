(ns gensql.query.permissive
  "Functions for issuing GenSQL-permissive queries, and converting them to strict
  queries."
  (:require #?(:clj [clojure.core.match :as match]
               :cljs [cljs.core.match :as match])
            [clojure.string :as string]
            [clojure.walk :as walk]
            [gensql.query.base :as base]
            [gensql.query.parser.tree :as tree]
            [gensql.query.permissive.parser :as parser]
            [gensql.query.sequence :as sequence]
            [gensql.query.string :as query.string]))

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

(def identifier-list?
  (tree/tag-pred :identifier-list))

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

(defn identifier->variable
  "Wraps an identifier node in a variable node."
  [node]
  [:variable "VAR" ws node])

(defn ^:private ands->commas
  "Replaces subsequent pairs of whitespace nodes followed by \"AND\" with
  a comma."
  [node]
  ;; Really awkward to do this with a loop, but reduce/partition alternatives
  ;; have the problem of not being able to look ahead easily, or skip over
  ;; elements. An index is actually easier.
  (let [cnt (count node)
        stop-idx (dec cnt)]
    (if (= 1 cnt)
      node
      (loop [i 0
             acc []]
        (cond (> i stop-idx) acc                            ; final pair was ws+and, so we overshot
              (= i stop-idx) (conj acc (peek node))         ; conj last elt
              :else (let [n1 (nth node i)
                          n2 (nth node (inc i))]
                      (if (and (tree/whitespace? n1)
                               (string? n2)
                               (re-matches #"(?i)AND" n2))
                        (recur (+ 2 i) (conj acc ","))      ; skip to after AND
                        (recur (inc i) (conj acc n1)))))))))

(defn identifier-list->variable-list
  [node]
  (-> (into [:variable-list]
            (map (fif identifier->variable (tree/tag-pred :identifier)))
            (rest node))
      ands->commas))

(defn identifier->density-event-eq
  "Given an identifier node, returns the density event for when the
  variable of that name equals the value held by that symbol in the
  environment.

  Returns the node unaltered if not an identifier."
  [node]
  (if-not (= :identifier (tree/tag node))
    node
    (let [variable (identifier->variable node)
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

    [[:density-event-eq ([:identifier _] :as id) equals scalar-expr]]
    [:density-event-eq (identifier->variable id) ws equals ws [:scalar-expr scalar-expr]]

    [[:density-event-eq scalar-expr equals ([:identifier _] :as id)]]
    [:density-event-eq [:scalar-expr scalar-expr] ws equals ws (identifier->variable id)]

    [[:distribution-event-binop ([:identifier _] :as id) binop scalar-expr]]
    [:distribution-event-binop (identifier->variable id) ws binop ws [:scalar-expr scalar-expr]]

    [[:distribution-event-binop scalar-expr binop ([:identifier _] :as id)]]
    [:distribution-event-binop [:scalar-expr scalar-expr] ws binop ws (identifier->variable id)]

    [[:generate-expr generate [:identifier-list & nodes] under model]]
    (let [variable-list (into [:variable-list]
                              (map (fif identifier->variable (tree/tag-pred :identifier)))
                              nodes)]
      [:generate-expr generate ws variable-list ws under ws model])

    [[:generate-except-clause except [:identifier-list & nodes]]]
    [:generate-except-clause except ws (identifier-list->variable-list (into [:identifier-list] nodes))]
    [[:generate-except-clause except "(" [:identifier-list & nodes] ")"]]
    [:generate-except-clause except ws "(" (identifier-list->variable-list (into [:identifier-list] nodes)) ")"]

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
      of (of-list :guard identifier-list?)
      with (with-list :guard identifier-list?)
      under model]]
    (let [approx (query.string/match-case "approximate" mutual)]
      [:approx-mutual-info-expr approx ws mutual ws info ws
       of ws (identifier-list->variable-list of-list) ws
       with ws (identifier-list->variable-list with-list) ws
       under ws model])

    [[:mutual-info-expr mutual info
      of (of-event :guard distribution-event?)
      with (with-event :guard distribution-event?)
      under model]]
    [:mutual-info-expr mutual ws info ws
     of ws of-event ws
     with ws with-event ws
     under ws model]

    [[:given-except-clause except given-except-id-list]]
    [:conditioned-by-except-clause except ws (identifier-list->variable-list given-except-id-list)]

    [[:given-expr [:model-expr model] _given [:star star]]]
    [:conditioned-by-expr [:model-expr model] ws "CONDITIONED" ws "BY" ws [:star star]]

    [[:given-expr [:model-expr model] _given [:star star] except-clause]]
    [:conditioned-by-expr [:model-expr model] ws "CONDITIONED" ws "BY" ws [:star star] ws except-clause]

    [[:given-expr [:model-expr model] _given events]]
    (transduce (map identifier->density-event-eq)
               (completing model-expr)
               model
               (tree/child-nodes events))

    [[:standalone-event-conjunction & _]]
    (and-node :density-event-and
              (map identifier->density-event-eq
                   (tree/child-nodes node)))

    [[:standalone-event-list & _]]
    (and-node :density-event-and
              (map identifier->density-event-eq
                   (tree/child-nodes node)))

    :else
    node))

(defn ->strict
  [node]
  (let [f (fn strictify-branch-node [x]
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
