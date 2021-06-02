(ns inferenceql.query.lang.select
  "Functions for evaluating SELECT expressions."
  (:require [clojure.set :as set]
            [datascript.core :as d]
            [inferenceql.query.collections :as coll]
            [inferenceql.query.datalog :as datalog]
            [inferenceql.query.lang.eval :as eval]
            [inferenceql.query.lang.select.plan :as plan]
            [inferenceql.query.lang.select.selections]
            [inferenceql.query.lang.select.where]
            [inferenceql.query.parser.tree :as tree]
            [net.cgrand.xforms :as xforms]))

(def default-compare compare)
(def default-keyfn :db/id)

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

(defn limit-xform
  [node env]
  (if node
    (let [limit (eval/eval-child node env :nat)]
      (take limit))
    (map identity)))

(defn order-xform
  "Returns a transducer that reorders `rows` based on the `:order-by-clause` node
  `node`. Will order by `:db/id` if `node` is `nil`."
  [node env]
  (if-not node
    (map identity)
    (let [keyfn (or (eval/eval-child node env :name)
                    default-keyfn)
          compare (if-let [compare-node (tree/get-node node :compare-expr)]
                    (case (tree/tag (tree/only-child-node compare-node))
                      :ascending compare
                      :descending #(compare %2 %1))
                    default-compare)]
      (xforms/sort-by keyfn compare))))

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
  (map #(into (with-meta (empty %) (meta %))
              (remove (comp #{:iql/no-value} val))
              %)))

(def private-attrs
  "Private Datalog attributes that should not be returned to callers."
  #{:db/id :iql/type})

(def remove-private-attrs-xform
  "A transducer that removes private Datalog attributes in maps."
  (map #(apply dissoc % private-attrs)))

(defmethod eval/eval :select-expr
  [node env]
  (let [{:keys [query inputs]} (plan/inputize (plan/plan node env) env)

        order-by-clause (tree/get-node node :order-by-clause)
        limit-clause    (tree/get-node node :limit-clause)

        order-xform  (order-xform order-by-clause env)
        limit-xform  (limit-xform limit-clause env)

        rows
        (into []
              (comp (partition-all 100)
                    (map datalog/db)
                    (map #(apply d/q query % (rest inputs)))
                    (map #(sort-by :db/id %))
                    cat
                    (map #(vary-meta % assoc :iql/columns (keys %)))
                    remove-placeholders-xform
                    order-xform
                    limit-xform
                    remove-private-attrs-xform)
              (first inputs))

        all-keys (or (some->> (get query :keys)
                              (map keyword))
                     (into []
                           (comp (map meta)
                                 (mapcat :iql/columns)
                                 (distinct))
                           rows))

        columns (remove private-attrs all-keys)]

    (vary-meta rows assoc :iql/columns columns)))
