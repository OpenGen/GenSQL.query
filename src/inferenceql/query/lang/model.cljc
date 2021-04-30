(ns inferenceql.query.lang.model
  "Functions for evaluating model expressions."
  (:require [inferenceql.inference.gpm :as gpm]
            [inferenceql.inference.search.crosscat :as crosscat]
            [inferenceql.query.collections :as coll]
            [inferenceql.query.gpm.subset :as subset]
            [inferenceql.query.lang.eval :as eval]
            [inferenceql.query.parser.tree :as tree]))

(defmethod eval/eval :variable-list
  [node env]
  (into []
        (map #(eval/eval % env))
        (tree/child-nodes node)))

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
