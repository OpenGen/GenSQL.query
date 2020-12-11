(ns inferenceql.query
  "This file defines functions for parsing, transforming, and executing IQL-SQL
  queries. The public API for this file is the functions are `q`, `pq`, and
  `query-plan`."
  (:refer-clojure :exclude [eval])
  #?(:clj (:require [inferenceql.query.io :as io])
     :cljs (:require-macros [inferenceql.query.io :as io]))
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [datascript.core :as d]
            [instaparse.core :as insta]
            [instaparse.combinators :as combinators]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.inference.gpm.proto :as gpm.proto]
            [inferenceql.query.datalog :as datalog]
            [inferenceql.query.math :as math]
            [inferenceql.query.node :as node]
            [inferenceql.query.parse-tree :as tree]
            [net.cgrand.xforms :as xforms]))

(def entity-var '?entity)

(def default-table :data)
(def default-model :model)
(def default-compare compare)
(def default-keyfn :db/id)

(defn safe-get
  [coll k]
  (if (contains? coll k)
    (get coll k)
    (throw (ex-info "Collection does not contain key"
                    {::error ::safe-get
                     ::coll coll
                     ::k k}))))

(defn all-keys
  [ms]
  (into []
        (comp (mapcat keys)
              (distinct))
        ms))

(defrecord ConstrainedGPM [gpm targets constraints]
  gpm.proto/GPM
  (logpdf [this logpdf-targets logpdf-constraints]
    (let [merged-targets (select-keys logpdf-targets targets)
          merged-constraints (merge constraints logpdf-constraints)]
      (gpm/logpdf gpm merged-targets merged-constraints)))

  (simulate [this simulate-targets simulate-constraints]
    (let [merged-targets (set/intersection (set targets) (set simulate-targets))
          merged-constraints (merge constraints simulate-constraints)]
      (gpm/simulate gpm merged-targets merged-constraints))))

(defn constrain
  "Constrains the provided generative probabilistic model such that it only
  simulates the provided targets, and is always subject to the provided
  constraints."
  [gpm targets constraints]
  (assert vector? targets)
  (assert map? constraints)
  (->ConstrainedGPM gpm targets constraints))

(def default-environment
  {`math/exp math/exp
   `merge merge
   `d/pull d/pull
   `gpm/logpdf gpm/logpdf

   `=  =
   `not= not=
   `>  >
   `>= >=
   `<  <
   `<= <=})

(def input-symbols
  (->> default-environment
       (set/map-invert)
       (map (juxt key (comp datalog/variable val)))
       (into {})))

(defn inputize
  "Modifies the provided query plan such that all the symbols that are in the
  default environment are provided as inputs."
  [query-plan env]
  (let [replaced-symbols (->> (select-keys (:query query-plan) [:find :where])
                              (tree-seq coll? seq)
                              (filter (set (keys default-environment)))
                              (distinct))
        input-names (zipmap (keys default-environment)
                            (map input-symbols
                                 (vals default-environment)))]
    (-> query-plan
        (update-in [:query] #(walk/postwalk-replace input-names %))
        (update-in [:query :in] into (map input-names replaced-symbols))
        (update-in [:inputs] into (map #(safe-get env %) replaced-symbols))
        (update-in [:query :where] #(walk/postwalk (fn [form]
                                                     (cond-> form
                                                       (and (coll? form)
                                                            (= 'or-join (first form)))
                                                       (datalog/add-free-variables)))
                                                   %)))))

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

;;; Core functions

(defmulti eval (fn eval-dispatch [node _] (node/tag node)))

(defmethod eval :default
  [node env]
  (let [children (tree/children node)
        child-nodes (tree/child-nodes node)]
    (cond (= 1 (count child-nodes))
          (eval (first child-nodes) env)

          (= 1 (count children))
          (first children))))

(def hierarchy
  (-> (make-hierarchy)
      (derive :probability-clause :logpdf-clause)
      (derive :density-clause :logpdf-clause)))

(defmulti datalog-clauses (fn [node _]
                            (node/tag node))
  :hierarchy #'hierarchy)

(defmethod datalog-clauses :default
  [node env]
  (let [child-nodes (tree/child-nodes node)]
    (if-not (= 1 (count child-nodes))
      (throw (ex-info "Datalog clauses for node is not defined" {:node node}))
      (datalog-clauses (first child-nodes) env))))

;;; Literals

(defmethod eval :string
  [node _]
  (edn/read-string (tree/only-child node)))

(defmethod eval :simple-symbol
  [node _]
  (edn/read-string (tree/only-child node)))

(defmethod eval :nat
  [node _]
  (edn/read-string (tree/only-child node)))

(defmethod eval :float
  [node _]
  (edn/read-string (tree/only-child node)))

(defmethod eval :int
  [node _]
  (edn/read-string (tree/only-child node)))

(defmethod eval :map-list
  [node env]
  (into []
        (map #(eval % env))
        (tree/child-nodes node)))

(defmethod eval :map-expr
  [node env]
  (into {}
        (map #(eval % env))
        (tree/child-nodes node)))

(defmethod eval :map-entry-expr
  [node env]
  (let [variable (eval (tree/get-node node :column-expr) env)
        value    (eval (tree/get-node node :value)       env)]
    {variable value}))

;;; Selections

(defn event-list-clauses
  "Given an `:event-list` node and a variable, returns a sequence of Datalog
  `:where` clauses that bind the values satisfying those events to the provided
  variable."
  [node variable env]
  (let [events-by-tag (group-by node/tag (tree/children node))
        column-names (mapv #(eval % env) (:column-expr events-by-tag))

        row-var (datalog/genvar "row-events-")
        row-clause (cond (some? (:star events-by-tag)) `[(d/pull ~'$ ~'[*]         ~entity-var) ~row-var]
                         (seq column-names)            `[(d/pull ~'$ ~column-names ~entity-var) ~row-var]
                         :else                         `[(~'ground {})                          ~row-var])

        binding-sym (datalog/genvar "binding-events-")
        binding-map (or (some->> (:map-entry-expr events-by-tag)
                                 (map #(eval % env))
                                 (reduce merge))
                        {})

        event-clause `[(~'ground ~binding-map) ~binding-sym]
        merge-clause `[(merge ~row-var ~binding-sym) ~variable]]
    [row-clause event-clause merge-clause]))

(defmethod datalog-clauses :logpdf-clause
  [node env]
  (let [model (or (some-> (tree/get-node-in node [:under-clause :model-expr])
                          (eval env))
                  (safe-get env default-model))

        key (or (some-> (tree/get-node-in node [:label-clause :name])
                        (eval env)
                        (name)
                        (symbol))
                (gensym "density"))

        log-density-var (datalog/variable (str "log-" key))
        density-var (datalog/variable key)

        model-var       (datalog/genvar "model-")
        target-var      (datalog/genvar "target-")
        constraints-var (datalog/genvar "constraints-")

        target-clauses (event-list-clauses (tree/get-node-in node [:of-clause :event-list])
                                           target-var
                                           env)
        constraints-clauses (event-list-clauses (tree/get-node-in node [:probability-given-clause :event-list])
                                                constraints-var
                                                env)

        logpdf-clauses `[[(gpm/logpdf ~model-var ~target-var ~constraints-var) ~log-density-var]
                         [(math/exp ~log-density-var) ~density-var]]]
    {:find   [density-var]
     :keys   [key]
     :in     [model-var]
     :inputs [model]
     :where  (reduce into [target-clauses constraints-clauses logpdf-clauses])}))

(defmethod datalog-clauses :column-selection
  [node env]
  (let [column (-> node
                   (tree/get-node :column-expr)
                   (eval env))
        key (symbol (or (some-> (tree/get-node-in node [:label-clause :name])
                                (eval env))
                        column))
        variable (datalog/genvar key)]
    {:find [variable]
     :keys [key]
     :where `[[(~'get-else ~'$ ~entity-var ~column :iql/no-value) ~variable]]}))

(defmethod datalog-clauses :rowid-selection
  [_ _]
  {:find '[?entity]
   :keys '[rowid]
   :where [[entity-var :iql/type :iql.type/row]]})

(defmethod datalog-clauses :select-clause
  [node env]
  (let [select-list (tree/get-node node :select-list)
        star-node (tree/get-node select-list :star)]
    (datalog/merge {:where [[entity-var :iql/type :iql.type/row]]}
                   (if star-node
                     {:find `[[(~'pull ~entity-var [~'*]) ~'...]]}
                     (->> (tree/child-nodes select-list)
                          (map #(datalog-clauses % env))
                          (apply datalog/merge {:find [entity-var]
                                                :keys ['db/id]}))))))

;;; Conditions

(defmethod datalog-clauses :from-clause
  [node env]
  (let [data-source (-> node
                        (tree/get-node :table-expr)
                        (eval env))]
    {:in ['$]
     :inputs [data-source]}))

(defmethod eval :alter-expr
  [node env]
  (let [table (-> (tree/get-node node :table-expr)
                  (eval env))
        column (-> (tree/get-node node :column-expr)
                   (eval env))]
    (map #(assoc % column :iql/no-value)
         table)))

(defmethod eval :insert-expr
  [node env]
  (let [table (-> (tree/get-node-in node [:insert-into-clause :table-expr])
                  (eval env))
        rows (-> (tree/get-node-in node [:values-clause :map-list])
                 (eval env))]
    (concat table rows)))

(defn set-function
  "Returns a function that, applies the changes described by the `SET` clause node
  `node` to its argument."
  [node env]
  (let [changes (-> (tree/get-node node :map-expr)
                    (eval env))]
    (fn [row]
      (merge row changes))))

(defn where-predicate
  "Takes a `:where-clause` node and returns a predicate that returns `true` if its
  argument satisfies the `WHERE` clause. Returns `false` otherwise."
  [node env]
  (let [query (datalog/merge `{:find [~entity-var]
                               :in [~'$]
                               :where [[~entity-var :iql/type :iql.type/row]]}
                             (datalog-clauses node env))]
    (fn [row]
      (let [db (datalog/db [row])
            {:keys [query inputs]} (inputize {:query query, :inputs [db]}
                                             env)]
        (boolean (seq (apply d/q query inputs)))))))

(defmethod eval :update-expr
  [node env]
  (let [table (-> (tree/get-node node :table-expr)
                  (eval env))
        f (set-function (tree/get-node node :set-clause)
                        env)
        pred (or (some-> (tree/get-node node :where-clause)
                         (where-predicate env))
                 (constantly true))]
    (mapv #(cond-> % (pred %) (f))
          table)))

(defmethod datalog-clauses :where-clause
  [node env]
  (->> (tree/child-nodes node)
       (map #(datalog-clauses % env))
       (apply datalog/merge)))

(defmethod datalog-clauses :presence-condition
  [node env]
  (let [{[sym] :find :as selection-clauses} (datalog-clauses (tree/get-node node :selection) env)]
    (datalog/merge (dissoc selection-clauses :find :keys)
                   {:where `[[(not= ~sym :iql/no-value)]]})))

(defmethod datalog-clauses :absence-condition
  [node env]
  (let [{[sym] :find :as selection-clauses} (datalog-clauses (tree/get-node node :selection) env)]
    (datalog/merge (dissoc selection-clauses :find :keys)
                   {:where `[[(= ~sym :iql/no-value)]]})))

(defmethod datalog-clauses :and-condition
  [node env]
  (let [child-clauses (->> (tree/child-nodes node)
                           (mapv #(datalog-clauses % env)))]
    (apply datalog/merge child-clauses)))

(defmethod datalog-clauses :equality-condition
  [node env]
  (let [{[variable] :find :as selection-clauses} (datalog-clauses (tree/get-node node :selection) env)
        value (eval (tree/get-node node :value)
                    env)]
    (datalog/merge (dissoc selection-clauses :find :keys)
                   {:where `[[(= ~variable ~value)]]})))

(defmethod datalog-clauses :or-condition
  [node env]
  (let [andify (fn [subclauses]
                 (if (= 1 (count subclauses))
                   (first subclauses)
                   `(~'and ~@subclauses)))
        subclauses (map #(datalog-clauses % env)
                        (tree/child-nodes node))]
    (assert (every? #(= [:where] (keys %)) subclauses))
    (let [where-subclauses (->> subclauses
                                (map :where)
                                (map andify))]
      {:where [`(~'or-join [~entity-var] ~@where-subclauses)]})))

(defmethod datalog-clauses :predicate-condition
  [node env]
  (let [lhs-node (tree/get-node node 0)
        {[sym] :find :as selection-clauses} (datalog-clauses lhs-node env)
        predicate (eval (tree/get-node-in node [:predicate-expr]) env)
        value     (eval (tree/get-node-in node [:value])          env)]
    (datalog/merge (dissoc selection-clauses :find :keys)
                   {:where `[[(not= ~sym :iql/no-value)]
                             [(~predicate ~sym ~value)]]})))

;;; Query execution

(defn plan
  "Given a `:select-expr` node returns a query plan for the top-most query.
  Subqueries will not be considered and are handled in a different step by the
  interpreter. See `q` for details."
  [node env]
  (let [default-from-clause (parse "FROM data" :start :from-clause)

        sql-select-clause (tree/get-node node :select-clause)
        sql-from-clause   (tree/get-node node :from-clause default-from-clause)
        sql-where-clause  (tree/get-node node :where-clause)

        datalog-select-clauses (datalog-clauses sql-select-clause env)
        datalog-from-clauses   (datalog-clauses sql-from-clause env)

        datalog-where-clauses  (if sql-where-clause
                                 (datalog-clauses sql-where-clause env)
                                 {})

        all-clauses (datalog/merge datalog-from-clauses ; data source comes first
                                   datalog-select-clauses
                                   datalog-where-clauses)

        inputs (get all-clauses :inputs)
        query (dissoc all-clauses :inputs)]
    {:query query
     :inputs inputs}))

(defmethod eval :variable-list
  [node env]
  (into []
        (map #(eval % env))
        (tree/child-nodes node)))

(defmethod eval :ref
  [node env]
  (let [k (-> node
              (tree/get-node :name)
              (eval env)
              (keyword))]
    (safe-get env k)))

(defmethod eval :predicate-expr
  [node _]
  (symbol #?(:clj "clojure.core"
             :cljs "cljs.core")
          (tree/only-child node)))

(defmethod eval :name
  [node env]
  (-> (tree/get-node node :simple-symbol)
      (eval env)
      (keyword)))

(defmethod eval :generate-expr
  [node env]
  (let [default-under-clause (parse "UNDER model" :start :under-clause)
        default-constraints {}

        model (-> node
                  (tree/get-node :under-clause default-under-clause)
                  (tree/get-node :model-expr)
                  (eval env))

        targets (-> node (tree/get-node :variable-list) (eval env))

        constraints (or (some-> node
                                (tree/get-node-in [:generate-given-clause :map-expr])
                                (eval env))
                        default-constraints)]
    (constrain model targets constraints)))

(defmethod eval :incorporate-expr
  [node env]
  (let [row (-> node
                (tree/get-node-in [:rows-clause :map-expr])
                (eval env))
        model (-> node
                  (tree/get-node-in [:incorporate-into-clause :model-expr])
                  (eval env))]
    (gpm/incorporate model row)))

(defmethod eval :generated-table-expr
  [node env]
  (let [{:keys [targets] :as model} (eval (tree/get-node node :generate-expr)
                                          env)]
    (repeatedly #(gpm/simulate model targets {}))))

(defmethod eval :ascending
  [_ _]
  compare)

(defmethod eval :descending
  [_ _]
  #(compare %2 %1))

;;; Post-processing xforms

(defn add-placeholders
  "Ensures that every map in `coll` has the same keys by filling in missing cells
  with the null placeholder."
  [coll]
  (let [columns (set/union (set (all-keys coll))
                           (set (:iql/columns (meta coll))))]
    (mapv #(merge (zipmap columns (repeat :iql/no-value))
                  %)
          coll)))

(def remove-placeholders-xform
  "A transducer that removes keys whose values are null placeholders"
  (map #(into {}
              (remove (comp #{:iql/no-value} val))
              %)))

(def private-attrs
  "Private Datalog attributes that should not be returned to callers."
  #{:db/id :iql/type})

(def remove-private-attrs-xform
  "A transducer that removes private Datalog attributes in maps."
  (map #(apply dissoc % private-attrs)))

;; Limit

(defn limit-xform
  [node env]
  (if node
    (let [nat-node (tree/get-node node :nat)
          limit (eval nat-node env)]
      (take limit))
    (map identity)))

;;; Order

(defn order-xform
  "Returns a transducer that reorders `rows` based on the `:order-by-clause` node
  `node`. Will order by `:db/id` if `node` is `nil`."
  [node env]
  (let [keyfn (or (some-> (tree/get-node node :name)
                          (eval env))
                  default-keyfn)
        compare (or (some-> (tree/get-node node :compare-expr)
                            (eval env))
                    default-compare)]
    (xforms/sort-by keyfn compare)))

;;; Evaluation

(defmethod eval :select-expr
  [node env]
  (let [{:keys [query inputs]} (inputize (plan node env) env)

        order-by-clause (tree/get-node node :order-by-clause)
        limit-clause    (tree/get-node node :limit-clause)

        order-xform  (order-xform order-by-clause env)
        limit-xform  (limit-xform limit-clause env)

        inputs (update inputs 0 #(datalog/db (into [] limit-xform %)))
        datalog-results (apply d/q query inputs)

        rows (into []
                   (comp remove-placeholders-xform
                         order-xform
                         limit-xform
                         remove-private-attrs-xform)
                   datalog-results)

        all-keys (or (some->> (get query :keys)
                              (map keyword))
                     (all-keys datalog-results))
        columns (remove private-attrs all-keys)]
    (vary-meta rows assoc :iql/columns columns)))

(defn q
  "Returns the result of executing a query on a set of rows. A registry
  mapping model names to model values models can be provided as an optional
  third argument."
  ([query rows]
   (q query rows {}))
  ([query rows models]
   (let [node-or-failure (parse query)]
     (if-not (insta/failure? node-or-failure)
       (let [rows (add-placeholders rows)
             env (merge default-environment models {default-table rows})]
         (eval node-or-failure env))
       (let [failure (insta/get-failure node-or-failure)
             ex-map {:cognitect.anomalies/category :cognitect.anomalies/incorrect
                     :instaparse/failure failure}]
         (throw (ex-info "Parsing failure" ex-map)))))))

(defmethod eval :with-map-entry-expr
  [node env]
  (let [k (-> (tree/get-node node :name)
              (eval env))
        v (-> (tree/get-node node :with-map-value-expr)
              (eval env))]
    {k v}))

(defmethod eval :with-map-expr
  [node env]
  (into {}
        (map #(eval % env))
        (tree/child-nodes node)))

(defmethod eval :with-expr
  [node env]
  (let [bindings (-> (tree/get-node node :with-map-expr)
                     (eval env))
        subexpr-node (tree/get-node node :with-sub-expr)]
    (eval subexpr-node (merge env bindings))))
