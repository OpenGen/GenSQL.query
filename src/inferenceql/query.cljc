(ns inferenceql.query
  "This file defines functions for parsing, transforming, and executing IQL-SQL
  queries. The public API for this file is the functions are `q`, `pq`, and
  `query-plan`."
  #?(:clj (:require [inferenceql.query.io :as io])
     :cljs (:require-macros [inferenceql.query.io :as io]))
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [datascript.core :as d]
            [instaparse.core :as insta]
            [instaparse.combinators :as combinators]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.datalog :as datalog]
            [inferenceql.query.gpm.subset :as subset]
            [inferenceql.query.lang.eval :as eval]
            [inferenceql.query.lang.constrain]
            [inferenceql.query.lang.literals]
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

;;; Selections

(defmethod eval/eval :event-list
  [node env]
  (let [nodes-by-tag (->> (tree/children node)
                          (group-by tree/tag))
        m (->> (:map-entry-expr nodes-by-tag)
               (map #(eval/eval % env))
               (into {}))
        ks (if (:star nodes-by-tag)
             keys
             (constantly
              (->> (:column-expr nodes-by-tag)
                   (map #(eval/eval % env)))))]
    (fn [env]
      (merge m (select-keys env (ks env))))))

(defmethod datalog-clauses :pdf-clause
  [node env]
  (let [key (or (some-> (eval/eval-child-in node env [:label-clause :name])
                        (name)
                        (symbol))
                (gensym "density"))
        target (-> (tree/get-node-in node [:of-clause :event-list])
                   (eval/eval env))
        pdf-var (datalog/variable (str key "-function"))
        pdf (fn [row]
              (let [env (-> env
                            (merge row)
                            (assoc ::row row))
                    model (if-let [model (eval/eval-child-in node env [:under-clause :model-expr])]
                            model
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
  (let [column (eval/eval-child node env :column-expr)
        key (symbol (or (eval/eval-child-in node env [:label-clause :name])
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
  (let [data-source (eval/eval-child node env :table-expr)]
    {:in ['$]
     :inputs [data-source]}))

(defmethod eval/eval :alter-expr
  [node env]
  (let [table (eval/eval-child node env :table-expr)
        column (eval/eval-child node env :column-expr)]
    (map #(assoc % column :iql/no-value)
         table)))

(defmethod eval/eval :insert-expr
  [node env]
  (let [table (eval/eval-child-in node env [:insert-into-clause :table-expr])
        rows (eval/eval-child-in node env [:values-clause :map-list])]
    (concat table rows)))

(defn set-function
  "Returns a function that, applies the changes described by the `SET` clause node
  `node` to its argument."
  [node env]
  (let [changes (eval/eval-child node env :map-expr)]
    (fn [row]
      (merge row changes))))

(defmethod eval/eval :update-expr
  [node env]
  (let [where-clauses (some-> (tree/get-node node :where-clause)
                              (datalog-clauses env))
        table (eval/eval-child node env :table-expr)
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
        value (eval/eval-child node env :value)]
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
        predicate (eval/eval-child node env :predicate-expr)
        value     (eval/eval-child node env :value)]
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

(defmethod eval/eval :variable-list
  [node env]
  (into []
        (map #(eval/eval % env))
        (tree/child-nodes node)))

(defmethod eval/eval :predicate-expr
  [node _]
  (symbol #?(:clj "clojure.core"
             :cljs "cljs.core")
          (tree/only-child node)))

(defmethod eval/eval :generate-expr
  [node env]
  (let [model (if-let [model (eval/eval-child-in node env [:under-clause :model-expr])]
                model
                (safe-get env :model))
        variables (let [variables-node (tree/get-node-in node [:generate-variables-clause 0])]
                    (case (tree/tag variables-node)
                      :star (gpm/variables model)
                      :variable-list (eval/eval variables-node env)))]
    (subset/subset-gpm model variables)))

(defmethod eval/eval :incorporate-expr
  [node env]
  (let [model (eval/eval-child-in node env [:incorporate-into-clause :model-expr])
        row-or-column-clause (tree/get-node-in node [:row-or-column-clause 0])]
    (case (tree/tag row-or-column-clause)
      :row-clause (let [row (eval/eval-child-in node env [:row-or-column-clause :row-clause :map-expr])]
                    (gpm/incorporate model row))
      :column-clause (let [column-clause (tree/get-node-in node [:row-or-column-clause :column-clause])
                           ;; TODO: use column-name when crosscat/incorporate-labels supports it.
                           _column-name (eval/eval-child column-clause env :label-clause)
                           column-values (eval/eval-child column-clause env :map-expr)
                           ;; rowids in iql.inference start at 0.
                           labels (reduce-kv (fn [accum k v]
                                               (assoc accum (dec k) v))
                                             {}
                                             column-values)]
                       (crosscat/incorporate-labels model labels)))))

(defmethod eval/eval :conditioned-by-expr
  [node env]
  (let [model (-> node
                  (tree/get-node :model-expr)
                  (eval/eval env))
        conditions (-> node
                       (tree/get-node :conditioning-event-expr)
                       (eval/eval env))]
    (gpm/condition model conditions)))

(defmethod eval/eval :conditioning-event-expr
  [node env]
  (let [[child-node] (tree/children node)
        m (eval/eval child-node env)]
    (medley/remove-vals #{:iql/no-value} m)))

(defmethod eval/eval :env-event-expr
  [node env]
  (let [k (eval/eval-child-in node env [:nilable-ref :name])]
    (if-let [
             v (-> (tree/only-child node)
                   (eval/eval env))]
      {k v}
      {})))

(defmethod eval/eval :conditioning-conjunction-expr
  [node env]
  (->> (tree/child-nodes node)
       (map #(eval/eval % env))
       (into {})))

(defmethod eval/eval :equivalence-relation-expr
  [node env]
  (let [[variable-expr value-expr] (tree/child-nodes node)
        variable (eval/eval variable-expr env)
        value (case (tree/tag value-expr)
                :variable-expr (let [var-name (eval/eval value-expr env)]
                                 (get env var-name))
                (eval/eval value-expr env))]
    {variable value}))

(defmethod eval/eval :generated-table-expr
  [node env]
  (let [model (eval/eval-child node env :generate-expr)]
    (repeatedly #(gpm/simulate model (gpm/variables model) {}))))

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
    (let [limit (eval/eval-child node env :nat)]
      (take limit))
    (map identity)))

;;; Order

(defn order-xform
  "Returns a transducer that reorders `rows` based on the `:order-by-clause` node
  `node`. Will order by `:db/id` if `node` is `nil`."
  [node env]
  (let [keyfn (or (eval/eval-child node env :name)
                  default-keyfn)
        compare (if-let [compare-node (tree/get-node node :compare-expr)]
                  (case (tree/tag (tree/only-child-node compare-node))
                    :ascending compare
                    :descending #(compare %2 %1))
                  default-compare)]
    (xforms/sort-by keyfn compare)))

;;; Evaluation

(defmethod eval/eval :select-expr
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
         (eval/eval node-or-failure env))
       (let [failure (insta/get-failure node-or-failure)
             ex-map {:cognitect.anomalies/category :cognitect.anomalies/incorrect
                     :instaparse/failure failure}]
         (throw (ex-info "Parsing failure" ex-map)))))))

(defmethod eval/eval :with-map-entry-expr
  [node env]
  (let [k (eval/eval-child node env :name)
        v (eval/eval-child node env :with-map-value-expr)]
    {k v}))

(defmethod eval/eval :with-map-expr
  [node env]
  (reduce (fn [env node]
            (merge env (eval/eval node env)))
          env
          (tree/child-nodes node)))

(defmethod eval/eval :with-expr
  [node env]
  (let [bindings (eval/eval-child node env :with-map-expr)]
    (eval/eval-child node (merge env bindings) :with-sub-expr)))
