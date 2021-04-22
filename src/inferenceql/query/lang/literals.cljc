(ns inferenceql.query.lang.literals
  (:require [clojure.edn :as edn]
            [inferenceql.query.lang.eval :as eval]
            [inferenceql.query.parse-tree :as tree]))

(defmethod eval/eval :string
  [node _]
  (edn/read-string (tree/only-child node)))

(defmethod eval/eval :simple-symbol
  [node _]
  (edn/read-string (tree/only-child node)))

(defmethod eval/eval :nat
  [node _]
  (edn/read-string (tree/only-child node)))

(defmethod eval/eval :float
  [node _]
  (edn/read-string (tree/only-child node)))

(defmethod eval/eval :int
  [node _]
  (edn/read-string (tree/only-child node)))

(defmethod eval/eval :bool
  [node _]
  (edn/read-string (tree/only-child node)))

(defmethod eval/eval :map-list
  [node env]
  (into []
        (map #(eval/eval % env))
        (tree/child-nodes node)))

(defmethod eval/eval :map-expr
  [node env]
  (into {}
        (map #(eval/eval % env))
        (tree/child-nodes node)))

(defmethod eval/eval :map-entry-expr
  [node env]
  (let [variable (eval/eval (tree/get-node node :key) env)
        value    (eval/eval (tree/get-node node :value) env)]
    {variable value}))
