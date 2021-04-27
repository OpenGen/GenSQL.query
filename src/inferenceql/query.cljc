(ns inferenceql.query
  "This file defines functions for parsing, transforming, and executing IQL-SQL
  queries. The public API for this file is the functions are `q`, `pq`, and
  `query-plan`."
  (:require [clojure.set :as set]
            [datascript.core :as d]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.collections :as coll]
            [inferenceql.query.datalog :as datalog]
            [inferenceql.query.gpm.subset :as subset]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.lang.eval :as eval]
            [inferenceql.query.lang.condition]
            [inferenceql.query.lang.constrain]
            [inferenceql.query.lang.literals]
            [inferenceql.query.lang.select.plan :as plan]
            [inferenceql.query.lang.table]
            [inferenceql.query.math :as math]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.inference.search.crosscat :as crosscat]
            [instaparse.core :as insta]
            [net.cgrand.xforms :as xforms]))

(def default-table :data)
(def default-model :model)
(def default-compare compare)
(def default-keyfn :db/id)

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

(defmethod plan/clauses :pdf-clause
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
                            (coll/safe-get env default-model))]
                (math/exp (gpm/logpdf model (target row) {}))))
        density-var (datalog/variable key)
        pdf-clause `[(~pdf-var ~plan/entity-var) ~density-var]]
    {:find   [density-var]
     :keys   [key]
     :in     [pdf-var]
     :inputs [pdf]
     :where  [pdf-clause]}))

(defmethod plan/clauses :column-selection
  [node env]
  (let [column (eval/eval-child node env :column-expr)
        key (symbol (or (eval/eval-child-in node env [:label-clause :name])
                        column))
        variable (datalog/genvar key)]
    {:find [variable]
     :keys [key]
     :where `[[(~'get-else ~'$ ~plan/eid-var ~column :iql/no-value) ~variable]]}))

(defmethod plan/clauses :rowid-selection
  [_ _]
  {:find [plan/eid-var]
   :keys '[rowid]
   :where [[plan/eid-var :iql/type :iql.type/row]]})

(defmethod plan/clauses :select-clause
  [node env]
  (let [select-list (tree/get-node node :select-list)
        star-node (tree/get-node select-list :star)]
    (datalog/merge {:where `[[~plan/eid-var :iql/type :iql.type/row]
                             [(d/entity ~'$ ~plan/eid-var) ~plan/entity-var]]}
                   (if star-node
                     {:find `[[(~'pull ~plan/eid-var [~'*]) ~'...]]}
                     (->> (tree/child-nodes select-list)
                          (map #(plan/clauses % env))
                          (apply datalog/merge {:find [plan/eid-var]
                                                :keys ['db/id]}))))))

(defmethod plan/clauses :from-clause
  [node env]
  (let [data-source (eval/eval-child node env :table-expr)]
    {:in ['$]
     :inputs [data-source]}))

(defmethod plan/clauses :where-clause
  [node env]
  (->> (tree/child-nodes node)
       (map #(plan/clauses % env))
       (apply datalog/merge)))

(defmethod plan/clauses :presence-condition
  [node env]
  (let [{[sym] :find :as selection-clauses} (plan/clauses (tree/get-node node :selection) env)]
    (datalog/merge (dissoc selection-clauses :find :keys)
                   {:where `[[(not= ~sym :iql/no-value)]]})))

(defmethod plan/clauses :absence-condition
  [node env]
  (let [{[sym] :find :as selection-clauses} (plan/clauses (tree/get-node node :selection) env)]
    (datalog/merge (dissoc selection-clauses :find :keys)
                   {:where `[[(= ~sym :iql/no-value)]]})))

(defmethod plan/clauses :and-condition
  [node env]
  (let [child-clauses (->> (tree/child-nodes node)
                           (mapv #(plan/clauses % env)))]
    (apply datalog/merge child-clauses)))

(defmethod plan/clauses :equality-condition
  [node env]
  (let [{[variable] :find :as selection-clauses} (plan/clauses (tree/get-node node :selection) env)
        value (eval/eval-child node env :value)]
    (datalog/merge (dissoc selection-clauses :find :keys)
                   {:where `[[(= ~variable ~value)]]})))

(defmethod plan/clauses :or-condition
  [node env]
  (let [andify (fn [subclauses]
                 (if (= 1 (count subclauses))
                   (first subclauses)
                   `(~'and ~@subclauses)))
        subclauses (map #(plan/clauses % env)
                        (tree/child-nodes node))]
    (assert (every? #(= [:where] (keys %)) subclauses))
    (let [where-subclauses (->> subclauses
                                (map :where)
                                (map andify))]
      {:where [`(~'or-join [~plan/eid-var] ~@where-subclauses)]})))

(defmethod plan/clauses :predicate-condition
  [node env]
  (let [lhs-node (tree/get-node node 0)
        {[sym] :find :as selection-clauses} (plan/clauses lhs-node env)
        predicate (eval/eval-child node env :predicate-expr)
        value     (eval/eval-child node env :value)]
    (datalog/merge (dissoc selection-clauses :find :keys)
                   {:where `[[(not= ~sym :iql/no-value)]
                             [(~predicate ~sym ~value)]]})))

;;; Query execution

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
                (coll/safe-get env :model))
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

;;; Post-processing xforms

(defn add-placeholders
  "Ensures that every map in `coll` has the same keys by filling in missing cells
  with the null placeholder."
  [coll]
  (let [columns (set/union (set (coll/all-keys coll))
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
  (let [{:keys [query inputs]} (plan/inputize (plan/plan node env) env)

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
                     (coll/all-keys datalog-results))
        columns (remove private-attrs all-keys)]
    (vary-meta rows assoc :iql/columns columns)))

(defn q
  "Returns the result of executing a query on a set of rows. A registry
  mapping model names to model values models can be provided as an optional
  third argument."
  ([query rows]
   (q query rows {}))
  ([query rows models]
   (let [node-or-failure (parser/parse query)]
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
