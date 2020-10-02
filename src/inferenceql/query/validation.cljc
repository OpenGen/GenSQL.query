(ns inferenceql.query.validation
  "Functions for validating queries."
  (:refer-clojure :exclude [ex-info])
  (:require [inferenceql.query :as query]
            [inferenceql.query.parse-tree :as tree]
            [inferenceql.query.node :as node]))

(def base-ex-info-map {:cognitect.anomalies/category :cognitect.anomalies/incorrect})

(defn ex-info
  "Like `clojure.core/ex-info`, but attaches information to `m` that is shared by
  all validation exceptions."
  [msg m]
  (clojure.core/ex-info msg (merge base-ex-info-map m)))

(defn select-without-limit
  "Returns an instance of `ExceptionInfo` if the top-level `:select-expr` in
  `node` selects from a generated table without also providing a limit."
  [node]
  (when-let [generated-table-expr (tree/get-node-in node [:from-clause :table-expr :generated-table-expr])]
    (when-not (tree/get-node node :limit-clause)
      (ex-info "Cannot SELECT from generated table without LIMIT"
               {:select-expr (query/unparse node)
                :generated-table-expr (query/unparse generated-table-expr)}))))

(defn non-data-table-ref
  "Returns an instance of `ExceptionInfo` if `node` selects from a table other
  than \"data\"."
  [node]
  (let [select-exprs (filter #(= :select-expr (node/tag %))
                             (tree-seq tree/branch? tree/children node))]
    (some (fn [select-expr]
            (when-let [ref (tree/get-node-in select-expr [:from-clause :table-expr :ref])]
              (when-not (= "data" (query/unparse ref))
                (ex-info "Cannot SELECT from table other than 'data'"
                         {:select-expr (query/unparse select-expr)
                          :table-expr (query/unparse ref)}))))
          select-exprs)))

(def validators [select-without-limit non-data-table-ref])

(defn error
  "Returns an instance of `ExceptionInfo` that describes the first validation
  failure in `:select-expr` `node`."
  [node]
  (some #(% node) validators))

(defn valid?
  "Returns `true` if `:select-expr` `node` is valid, `false` otherwise. "
  [node]
  (nil? (some #(% node) validators)))
