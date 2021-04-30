(ns inferenceql.query.lang.with-as
  "Functions for evaluating WITH expressions."
  (:require [inferenceql.query.lang.eval :as eval]
            [inferenceql.query.parser.tree :as tree]))

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
