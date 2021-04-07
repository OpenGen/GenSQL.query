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
            [inferenceql.inference.search.crosscat :as crosscat]
            [medley.core :as medley]
            [net.cgrand.xforms :as xforms]))

(def eid-var '?e)
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

(defrecord ConditionedGPM [gpm targets conditions]
  gpm.proto/GPM
  (logpdf [_ logpdf-targets logpdf-conditions]
    (let [merged-targets (select-keys logpdf-targets targets)
          merged-conditions (merge conditions logpdf-conditions)]
      (gpm/logpdf gpm merged-targets merged-conditions)))

  (simulate [_ simulate-targets simulate-conditions]
    (let [merged-targets (set/intersection (set targets) (set simulate-targets))
          merged-conditions (merge conditions simulate-conditions)]
      (gpm/simulate gpm merged-targets merged-conditions)))

  gpm.proto/Variables
  (variables [_]
    (set/intersection targets (gpm/variables gpm))))

(defn condition
  "Conditions the provided generative probabilistic model such that it only
  simulates the provided targets, and is always subject to the provided
  conditions."
  [gpm targets conditions]
  (assert vector? targets)
  (assert map? conditions)
  (->ConditionedGPM gpm targets conditions))

(def default-environment
  {`math/exp math/exp
   `merge merge
   `d/entity d/entity
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
      (derive :probability-clause :pdf-clause)
      (derive :density-clause :pdf-clause)))

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

(defmethod eval :bool
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
  (let [variable (eval (tree/get-node node :key) env)
        value    (eval (tree/get-node node :value) env)]
    {variable value}))

;;; Selections

(defmethod eval :event-list
  [node env]
  (let [nodes-by-tag (->> (tree/children node)
                          (group-by tree/tag))
        m (->> (:map-entry-expr nodes-by-tag)
               (map #(eval % env))
               (into {}))
        ks (if (:star nodes-by-tag)
             keys
             (constantly
              (->> (:column-expr nodes-by-tag)
                   (map #(eval % env)))))]
    (fn [env]
      (merge m (select-keys env (ks env))))))

(defmethod datalog-clauses :pdf-clause
  [node env]
  (let [key (or (some-> (tree/get-node-in node [:label-clause :name])
                        (eval env)
                        (name)
                        (symbol))
                (gensym "density"))
        target (-> (tree/get-node-in node [:of-clause :event-list])
                   (eval env))
        pdf-var (datalog/variable (str key "-function"))
        pdf (fn [row]
              (let [env (-> env
                            (merge row)
                            (assoc ::row row))
                    model (if-let [node (tree/get-node-in node [:under-clause :model-expr])]
                            (eval node env)
                            (safe-get env default-model))]
                (math/exp (gpm/logpdf model (target row) {}))))
        density-var (datalog/variable key)
        pdf-clause `[(~pdf-var ~entity-var) ~density-var]]
    {:find   [density-var]
     :keys   [key]
     :in     [pdf-var]
     :inputs [pdf]
     :where  [pdf-clause]}))

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
     :where `[[(~'get-else ~'$ ~eid-var ~column :iql/no-value) ~variable]]}))

(defmethod datalog-clauses :rowid-selection
  [_ _]
  {:find [eid-var]
   :keys '[rowid]
   :where [[eid-var :iql/type :iql.type/row]]})

(defmethod datalog-clauses :select-clause
  [node env]
  (let [select-list (tree/get-node node :select-list)
        star-node (tree/get-node select-list :star)]
    (datalog/merge {:where `[[~eid-var :iql/type :iql.type/row]
                             [(d/entity ~'$ ~eid-var) ~entity-var]]}
                   (if star-node
                     {:find `[[(~'pull ~eid-var [~'*]) ~'...]]}
                     (->> (tree/child-nodes select-list)
                          (map #(datalog-clauses % env))
                          (apply datalog/merge {:find [eid-var]
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

(defmethod eval :update-expr
  [node env]
  (let [where-clauses (some-> (tree/get-node node :where-clause)
                              (datalog-clauses env))
        table (-> (tree/get-node node :table-expr)
                  (eval env))
        f (set-function (tree/get-node node :set-clause)
                        env)]
    (if-not where-clauses
      (mapv f table)
      (let [query (datalog/merge `{:find [[~eid-var ...]]
                                   :in [~'$]
                                   :where [[~eid-var :iql/type :iql.type/row]]}
                                 where-clauses)
            db (datalog/db table)
            {:keys [query inputs]} (inputize {:query query :inputs [db]}
                                             env)
            affected-eids (apply d/q query inputs)]
        (reduce (fn [table eid]
                  (update table (dec eid) f))
                (vec table)
                affected-eids)))))

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
      {:where [`(~'or-join [~eid-var] ~@where-subclauses)]})))

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
  (if-let [x (-> (tree/get-node node :nilable-ref)
                 (eval env))]
    x
    (throw (ex-info (str "Unable to resolve symbol: " (unparse node) " in this context")
                    {:cognitect.anomalies/category :incorrect}))))

(defmethod eval :nilable-ref
  [node env]
  (let [ks (->> (tree/child-nodes node)
                (map #(eval % env)))]
    (get-in env ks)))

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
  (let [model (if-let [model-node (tree/get-node-in node [:under-clause :model-expr])]
                (eval model-node env)
                (safe-get env :model))
        targets (let [variables-node (tree/get-node-in node [:generate-variables-clause 0])]
                  (case (tree/tag variables-node)
                    :star (gpm/variables model)
                    :variable-list (eval variables-node env)))]
    (condition model targets {})))

(defmethod eval :incorporate-expr
  [node env]
  (let [model (-> node
                  (tree/get-node-in [:incorporate-into-clause :model-expr])
                  (eval env))
        row-or-column-clause (tree/get-node-in node [:row-or-column-clause 0])]
    (case (tree/tag row-or-column-clause)
      :row-clause (let [row (-> node
                                (tree/get-node-in [:row-or-column-clause :row-clause :map-expr])
                                (eval env))]
                    (gpm/incorporate model row))
      :column-clause (let [column-clause (tree/get-node-in node
                                                           [:row-or-column-clause :column-clause])
                           ;; TODO: use column-name when crosscat/incorporate-labels supports it.
                           _column-name (-> column-clause
                                            (tree/get-node :label-clause)
                                            (eval env))
                           column-values (-> column-clause
                                             (tree/get-node :map-expr)
                                             (eval env))
                           ;; rowids in iql.inference start at 0.
                           labels (reduce-kv (fn [accum k v]
                                               (assoc accum (dec k) v))
                                             {}
                                             column-values)]
                       (crosscat/incorporate-labels model labels)))))

(defmethod eval :conditioned-by-expr
  [node env]
  (let [model (-> node
                  (tree/get-node :model-expr)
                  (eval env))
        constraints (-> node
                        (tree/get-node :conditioning-event-expr)
                        (eval env))]
    (condition model (gpm/variables model) constraints)))

(defmethod eval :conditioning-event-expr
  [node env]
  (let [[child-node] (tree/children node)
        m (eval child-node env)]
    (medley/remove-vals #{:iql/no-value} m)))

(defmethod eval :env-event-expr
  [node env]
  (let [k (-> (tree/get-node-in node [:nilable-ref :name])
              (eval env))]
    (if-let [
             v (-> (tree/only-child node)
                   (eval env))]
      {k v}
      {})))

(defmethod eval :conditioning-conjunction-expr
  [node env]
  (->> (tree/child-nodes node)
       (map #(eval % env))
       (into {})))

(defmethod eval :equivalence-relation-expr
  [node env]
  (let [[variable-expr value-expr] (tree/child-nodes node)
        variable (eval variable-expr env)
        value (case (tree/tag value-expr)
                :variable-expr (let [var-name (eval value-expr env)]
                                 (get env var-name))
                (eval value-expr env))]
    {variable value}))

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
  (reduce (fn [env node]
            (merge env (eval node env)))
          env
          (tree/child-nodes node)))

(defmethod eval :with-expr
  [node env]
  (let [bindings (-> (tree/get-node node :with-map-expr)
                     (eval env))
        subexpr-node (tree/get-node node :with-sub-expr)]
    (eval subexpr-node (merge env bindings))))
