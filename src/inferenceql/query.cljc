(ns inferenceql.query
  "This file defines functions for parsing, transforming, and executing IQL-SQL
  queries. The public API for this file is the functions are `q`, `pq`, and
  `query-plan`."
  (:refer-clojure :exclude [eval])
  #?(:clj (:require [inferenceql.query.io :as io])
     :cljs (:require-macros [inferenceql.query.io :as io]))
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [datascript.core :as d]
            [instaparse.core :as insta]
            [instaparse.combinators :as combinators]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.inference.gpm.proto :as gpm.proto]
            [inferenceql.query.datalog :as datalog]
            [inferenceql.query.math :as math]
            [inferenceql.query.node :as node]
            [inferenceql.query.parse-tree :as tree]))

(def entity-var '?entity)
(def default-model-key :model)

(defn safe-get
  [coll k]
  (if (contains? coll k)
    (get coll k)
    (throw (ex-info "Collection does not contain key"
                    {::error ::safe-get
                     ::coll coll
                     ::k k}))))

(defn variable
  "Converts a string, symbol, or keyword to a valid Datalog variable of the same
  name."
  [x]
  ;; Not using a protocol here for now to avoid having to deal with differing
  ;; types in Clojure and ClojureScript.
  (cond (string? x) (symbol (cond->> x
                              (not (string/starts-with? x "?"))
                              (str "?")))
        (symbol? x) (variable (name x))
        (keyword? x) (variable (name x))))

(defn genvar
  "Like `gensym`, but generates Datalog variables."
  ([]
   (variable (gensym)))
  ([prefix-string]
   (variable (gensym prefix-string))))

(defn genvar?
  "Returns `true` if `var` was generated with `genvar`."
  [var]
  (string/starts-with? (name var) "?G__"))

(defn constrain
  "Constrains the provided generative probabilistic model such that it only
  simulates the provided targets, and is always subject to the provided
  constraints."
  [gpm constrain-targets constrain-constraints]
  (assert vector? constrain-targets)
  (assert map? constrain-constraints)

  (reify gpm.proto/GPM
    (logpdf [this logpdf-targets logpdf-constraints]
      (let [merged-targets (select-keys logpdf-targets constrain-targets)
            merged-constraints (merge constrain-constraints logpdf-constraints)]
        (gpm/logpdf gpm merged-targets merged-constraints)))

    (simulate [this simulate-targets simulate-constraints]
      (let [merged-targets (set/intersection (set constrain-targets) (set simulate-targets))
            merged-constraints (merge constrain-constraints simulate-constraints)]
        (gpm/simulate gpm merged-targets merged-constraints)))))

(def default-environment
  {`math/exp math/exp
   `merge merge
   `d/pull d/pull
   `gpm/logpdf gpm/logpdf

   `=  =
   `>  >
   `>= >=
   `<  <
   `<= <=})

(def input-symbols
  (->> default-environment
       (set/map-invert)
       (map (juxt key (comp variable val)))
       (into {})))

;;; Parsing

(def bnf (io/inline-file "inferenceql/query/grammar.bnf"))

(def parse
  "An instaparse parser for IQL SQL queries. The grammar is inlined at macro
  expansion time so that it can be used in the ClojureScript context where we
  don't have access to file resources."
  (insta/parser bnf))

(def non-terminals (set (keys (combinators/ebnf bnf))))

(def unparse-transformations (zipmap non-terminals (repeat str)))

(defn unparse
  "Returns a string that when parsed by `parse` will yield the provided parse tree."
  [node]
  (insta/transform unparse-transformations node))

;;; Literal transformation

(def literal-transformations
  "An `instaparse.core/transform` transform map that transforms literals into
  their corresponding values."
  {:string edn/read-string
   :symbol edn/read-string
   :nat    edn/read-string
   :float  edn/read-string
   :int    edn/read-string})

(defn transform
  "Transforms an InferenceQL parse tree into a map with the same information, but
  in a format that is easier to work with. The output of this function is
  consumed by `execute`."
  [node]
  (let [all-transformations (merge literal-transformations
                                   {:name keyword})]
    (insta/transform all-transformations node)))

;;; Selections

(defn events-clauses
  "Given a variable and a collection of events, returns a sequence of Datalog
  `:where` clauses that bind the values satisfying those events to the provided
  variable."
  [variable events]
  (let [row-var (genvar "row-events-")
        {column-nodes :column-name binding-nodes :binding star :star} (group-by node/tag events)
        column-names (mapv (comp first tree/children) column-nodes)

        row-clause (cond (some? star)       `[(d/pull ~'$ ~'[*]         ~entity-var) ~row-var]
                         (seq column-names) `[(d/pull ~'$ ~column-names ~entity-var) ~row-var]
                         :else              `[(~'ground {})                          ~row-var])

        binding-sym (genvar "binding-events-")
        binding-map (or (some->> binding-nodes
                                 (map (fn [node]
                                        (let [variable (tree/find-child-in node [:column-name])
                                              value    (tree/find-child-in node [:value])]
                                          {variable value})))
                                 (reduce merge))
                        {})

        event-clause `[(~'ground ~binding-map) ~binding-sym]
        merge-clause `[(merge ~row-var ~binding-sym) ~variable]]
    [row-clause event-clause merge-clause]))

(def hierarchy
  (-> (make-hierarchy)
      (derive :probability-of :logpdf-of)
      (derive :density-of :logpdf-of)))

(defmulti datalog-clauses node/tag :hierarchy #'hierarchy)

(defmethod datalog-clauses :logpdf-of
  [node]
  (let [model (or (tree/find-child-node-in node [:model])
                  (transform (parse "model" :start :lookup)))

        selection-name (or (tree/find-child-in node [:selection-name])
                           (keyword (gensym "density")))

        log-density-var (variable (str "log-" (name selection-name)))
        density-var (variable selection-name)

        model-var       (genvar "model-")
        target-var      (genvar "target-")
        constraints-var (genvar "constraints-")

        target-clauses      (events-clauses target-var      (tree/find-children-in node [:target      :events]))
        constraints-clauses (events-clauses constraints-var (tree/find-children-in node [:constraints :events]))

        logpdf-clauses `[[(gpm/logpdf ~model-var ~target-var ~constraints-var) ~log-density-var]
                         [(math/exp ~log-density-var) ~density-var]]]
    {:name   [selection-name]
     :find   [density-var]
     :in     [model-var]
     :inputs [model]
     :where  (reduce into [target-clauses constraints-clauses logpdf-clauses])}))

(defmethod datalog-clauses :column-selection
  [node]
  (let [column (tree/find-child-in node [:column-name])
        name (name (or (tree/find-child-in node [:selection-name])
                       column))
        variable (variable name)]
    {:name [(keyword name)]
     :find [variable]
     :where `[[(~'get-else ~'$ ~entity-var ~column :iql/no-value) ~variable]]}))

(defmethod datalog-clauses :selections
  [node]
  (merge-with into {:where [[entity-var :iql/type :iql.type/row]]}
              (if (tree/find-tag node :star)
                {:find `[[(~'pull ~entity-var [~'*]) ~'...]]}
                (->> (tree/children node)
                     (filter #(contains? #{:column-selection :probability-of :density-of} (node/tag %)))
                     (map datalog-clauses)
                     (apply merge-with into {:find [entity-var]})))))

;;; Conditions

(defn add-free-variables
  "Given an `or-join` form like

    (or-join <join-vars> <subcond1> <subcond2>)

  adds to `<join-vars>` the variables from the subclauses that were not generated
  with `genvar`. Variables generated with `genvar` are presumed to not be needed
  outside the `or-join`."
  [form]
  (let [free-variables (into []
                             (comp (remove genvar?)
                                   (distinct))
                             (datalog/free-variables form))]
    (-> (vec form)
        (update 1 into free-variables)
        (update 1 distinct)
        (update 1 vec)
        (seq))))

(defmethod datalog-clauses :conditions
  [node]
  (->> (tree/children node)
       (filter tree/branch?)
       (map datalog-clauses)
       (apply merge-with into)))

(defmethod datalog-clauses :presence-condition
  [node]
  (let [attribute (tree/find-child-in node [:column-name])]
    {:where [[entity-var attribute '_]]}))

(defmethod datalog-clauses :absence-condition
  [node]
  (let [attribute (tree/find-child-in node [:column-name])]
    {:where `[[(~'missing? ~'$ ~entity-var ~attribute)]]}))

(defmethod datalog-clauses :and-condition
  [node]
  (->> (tree/children node)
       (filter tree/branch?)
       (map datalog-clauses)
       (apply merge-with into)))

(defmethod datalog-clauses :equality-condition
  [node]
  (let [attribute (tree/find-child-in node [:column-name])
        value (tree/find-child-in node [:value])]
    {:where [[entity-var attribute value]]}))

(defmethod datalog-clauses :or-condition
  [node]
  (let [andify (fn [subclauses]
                 (if (= 1 (count subclauses))
                   (first subclauses)
                   `(~'and ~@subclauses)))
        subclauses (->> (tree/children node)
                        (filter tree/branch?)
                        (map datalog-clauses))]
    (assert (every? #(= [:where] (keys %)) subclauses))
    (let [where-subclauses (->> subclauses
                                (map :where)
                                (map andify))]
      {:where [(add-free-variables
                `(~'or-join [] ~@where-subclauses))]})))

(defmethod datalog-clauses :predicate-condition
  [node]
  (let [sym (genvar)
        column    (tree/find-child-in node [:column-name])
        predicate (tree/find-child-in node [:predicate])
        value     (tree/find-child-in node [:value])
        function (symbol #?(:clj "clojure.core"
                            :cljs "cljs.core")
                         (name predicate))]
    {:where `[~[entity-var column sym]
              [(~function ~sym ~value)]]}))

;;; Query execution

(defn inputize
  "Modifies the provided query plan such that all the symbols that are in the
  default environment are provided as inputs."
  [query-plan]
  (let [lookup-node #(tree/node :lookup [%])
        replaced-symbols (->> (select-keys (:query query-plan) [:find :where])
                              (tree-seq coll? seq)
                              (filter (set (keys default-environment)))
                              (distinct))
        input-names (zipmap (keys default-environment)
                            (map input-symbols
                                 (vals default-environment)))]
    (-> query-plan
        (update-in [:query] #(walk/postwalk-replace input-names %))
        (update-in [:query :in] into (map input-names replaced-symbols))
        (update-in [:inputs] into (map lookup-node replaced-symbols))
        (update-in [:query :where] #(walk/postwalk (fn [form]
                                                     (cond-> form
                                                       (and (coll? form)
                                                            (= 'or-join (first form)))
                                                       (add-free-variables)))
                                                   %)))))

;; TODO: Add validation disallowing generate without limit
;; TODO: Add validation requiring that the source table be "data"

(defn plan
  "Given a query node returns a query plan for the top-most query.
  Subqueries will not be considered and are handled in a different step by the
  interpreter. See `q` for details."
  [node]
  (let [{sel-find :find sel-in :in sel-where :where sel-inputs :inputs} (datalog-clauses (tree/find-tag node :selections))
        source-node (or (tree/find-child-in node [:source])
                        (-> (parse "data" :start :lookup)
                            (transform)))
        condition-nodes (filter tree/branch? (tree/find-children-in node [:conditions]))
        cond-where (mapcat (comp :where datalog-clauses) condition-nodes)]
    {:query {:find sel-find
             :in (into '[$] sel-in)
             :where (into sel-where cond-where)}
     :inputs (into [source-node] sel-inputs)}))

(defn iql-db
  "Converts a vector of maps into Datalog database that can be queried with `q`."
  [rows]
  (let [facts (map #(assoc % :iql/type :iql.type/row)
                   rows)]
    (d/db-with (d/empty-db) facts)))

(defmulti eval (fn [node _] (node/tag node)))

(defmethod eval :binding
  [node _]
  (let [variable (tree/find-child-in node [:column-name])
        value    (tree/find-child-in node [:value])]
    {variable value}))

(defmethod eval :bindings
  [node env]
  (transduce (map #(eval % env))
             merge
             (tree/find-all-tag node :binding)))

(defmethod eval :variable-name
  [node _]
  (first (tree/children node)))

(defmethod eval :variable-names
  [node env]
  (mapv #(eval % env) (tree/find-all-tag node :variable-name)))

(defmethod eval :lookup
  [node env]
  (get env (first (tree/children node))))

(defmethod eval :generate-model
  [node env]
  (let [model (if-let [model-node (tree/find-child-in node [:model])]
                (eval model-node env)
                (safe-get env :model))
        targets (-> node
                    (tree/find-in [:variable-names])
                    (eval env))
        constraints (or (some-> node
                                (tree/find-in [:bindings])
                                (eval env))
                        {})]
    (constrain model targets constraints)))

(defmethod eval :generated-table
  [node env]
  (let [target (eval (tree/find-in node [:variable-names])
                     env)
        constraints (or (some-> (tree/find-in node [:bindings])
                                (eval env))
                        {})
        model (eval (tree/find-child-node-in node [:model])
                    env)]
    (repeatedly #(gpm/simulate model target constraints))))

(defn order
  "Reorders `rows` based on the `:ordering` node `node`. Will order by `:db/id` if
  `node` is `nil`."
  [node rows]
  (let [keyfn (or (keyword (tree/find-child-in node [:column-name]))
                  :db/id)
        cmp (case (node/tag (tree/find-child-in node [:direction]))
              :ascending compare
              :descending #(compare %2 %1)
              nil compare)]
    (sort-by keyfn cmp rows)))

(defmethod eval :query
  [node env]
  (let [node (transform node)
        names (-> (tree/find-tag node :selections)
                  (datalog-clauses)
                  (:name)) ; TODO: fix redundant call to selections-clauses
        {query :query input-nodes :inputs} (inputize (plan node))
        limit (tree/find-child-in node [:limit])
        inputs (update (mapv #(eval % env) input-nodes)
                       0
                       #(iql-db (cond->> % limit (take limit))))
        rows (cond->> (apply d/q query inputs)
               names (map #(zipmap (into [:db/id] names) ; TODO: Can this not be hard-coded?
                                   %))
               :always (order (tree/find-tag node :ordering))
               :always (map #(into {}
                                   (remove (comp #{:iql/no-value} val))
                                   %))
               :always (map #(dissoc % :db/id :iql/type))
               limit (take limit))
        metadata (or names
                     (into []
                           (comp (mapcat keys)
                                 (distinct))
                           rows))]
    (vary-meta rows assoc :iql/columns metadata)))

(defn q
  "Returns the result of executing a query on a set of rows. A registry
  mapping model names to model values models can be provided as an optional
  third argument."
  ([query rows]
   (q query rows {}))
  ([query rows models]
   (let [node-or-failure (parse query)]
     (if-not (insta/failure? node-or-failure)
       (let [env (merge default-environment models {:data rows})]
         (eval node-or-failure env))
       (let [failure (insta/get-failure node-or-failure)
             ex-map {:cognitect.anomalies/category :cognitect.anomalies/incorrect
                     :instaparse/failure failure}]
         (throw (ex-info "Parsing failure" ex-map)))))))
