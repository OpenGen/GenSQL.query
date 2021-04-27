(ns inferenceql.query.lang.eval
  "Functions for evaluating parse tree nodes. The most important function in
  this namespace is the multimethod `inferenceql.query.lang.eval/eval` which
  accepts a parse tree node and evalutes it in an environment.  Implementations
  of `inferenceql.query.lang.eval/eval` are defined in several other
  namespaces."
  (:refer-clojure :exclude [eval])
  (:require [inferenceql.query.node :as node]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.parser.tree :as tree]))

(defmulti eval (fn eval-dispatch [node _] (node/tag node)))

(defn eval-child-in
  "Like `inferenceql.query.parser.tree/get-node-in`, but evaluates the node."
  [node env tags]
  (when-let [child (tree/get-node-in node tags)]
    (eval child env)))

(defn eval-child
  "Like `inferenceql.query.parser.tree/get-node`, but evaluates the node."
  [node env tag]
  (eval-child-in node env [tag]))

(defmethod eval :default
  [node env]
  (let [children (tree/children node)
        child-nodes (tree/child-nodes node)]
    (cond (= 1 (count child-nodes))
          (eval (first child-nodes) env)

          (= 1 (count children))
          (first children))))

(defmethod eval :ref
  [node env]
  (if-let [x (eval-child node env :nilable-ref)]
    x
    (throw (ex-info (str "Unable to resolve symbol: " (parser/unparse node) " in this context")
                    {:cognitect.anomalies/category :incorrect}))))

(defmethod eval :nilable-ref
  [node env]
  (let [ks (->> (tree/child-nodes node)
                (map #(eval % env)))]
    (get-in env ks)))

(defmethod eval :name
  [node env]
  (-> (eval-child node env :simple-symbol)
      (keyword)))
