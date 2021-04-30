(ns inferenceql.query
  "This file defines functions for parsing, transforming, and executing IQL-SQL
  queries. The public API for this file is the functions are `q`, `pq`, and
  `query-plan`."
  (:require [datascript.core :as d]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.collections :as coll]
            [inferenceql.query.gpm.subset :as subset]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.lang.eval :as eval]
            [inferenceql.query.lang.condition]
            [inferenceql.query.lang.constrain]
            [inferenceql.query.lang.literals]
            [inferenceql.query.lang.select :as select]
            [inferenceql.query.lang.table]
            [inferenceql.query.lang.with-as]
            [inferenceql.query.math :as math]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.inference.search.crosscat :as crosscat]
            [instaparse.core :as insta]))

(def default-table :data)

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

;;; Query execution

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

(defn q
  "Returns the result of executing a query on a set of rows. A registry
  mapping model names to model values models can be provided as an optional
  third argument."
  ([query rows]
   (q query rows {}))
  ([query rows models]
   (let [node-or-failure (parser/parse query)]
     (if-not (insta/failure? node-or-failure)
       (let [rows (select/add-placeholders rows)
             env (merge default-environment models {default-table rows})]
         (eval/eval node-or-failure env))
       (let [failure (insta/get-failure node-or-failure)
             ex-map {:cognitect.anomalies/category :cognitect.anomalies/incorrect
                     :instaparse/failure failure}]
         (throw (ex-info "Parsing failure" ex-map)))))))
