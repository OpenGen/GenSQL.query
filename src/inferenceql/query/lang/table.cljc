(ns inferenceql.query.lang.table
  "Functions for evaluating table expressions. This namespace is not exhaustive;
  some table expressions that are sufficiently complicated have their own
  namespaces."
  (:require [datascript.core :as d]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.datalog :as datalog]
            [inferenceql.query.lang.eval :as eval]
            [inferenceql.query.lang.select.plan :as plan]
            [inferenceql.query.parser.tree :as tree]))

(defmethod eval/eval :alter-expr
  [node env]
  (let [table (eval/eval-child node env :table-expr)
        column (eval/eval-child node env :column-expr)]
    (map #(assoc % column :iql/no-value)
         table)))

(defmethod eval/eval :generated-table-expr
  [node env]
  (let [model (eval/eval-child node env :generate-expr)]
    (repeatedly #(gpm/simulate model (gpm/variables model) {}))))

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
                              (plan/clauses env))
        table (eval/eval-child node env :table-expr)
        f (set-function (tree/get-node node :set-clause)
                        env)]
    (if-not where-clauses
      (mapv f table)
      (let [query (datalog/merge `{:find [[~plan/eid-var ...]]
                                   :in [~'$]
                                   :where [[~plan/eid-var :iql/type :iql.type/row]]}
                                 where-clauses)
            db (datalog/db table)
            {:keys [query inputs]} (plan/inputize {:query query :inputs [db]}
                                                  env)
            affected-eids (apply d/q query inputs)]
        (reduce (fn [table eid]
                  (update table (dec eid) f))
                (vec table)
                affected-eids)))))
