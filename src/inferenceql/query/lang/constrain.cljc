(ns inferenceql.query.lang.constrain
  (:require [clojure.walk :as walk]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.lang.eval :as eval]
            [inferenceql.query.parser.tree :as tree]))

(defn ^:private event->sexpr
  "Takes a :constrain-event-expr node and returns a s-expression with the same
  structure. For instance, if the node was parsed from the string \"x=3\" the
  value returned will be '(= :x 3)."
  [node]
  (let [f (fn [node]
            (if-not (tree/branch? node)
              node
              (case (tree/tag node)
                :constrain-event-expr (recur (tree/only-child-node node))
                :constrain-conjunction-expr (cons 'and (tree/child-nodes node))
                :constrain-disjunction-expr (cons 'or (tree/child-nodes node))
                :binary-relation-expr (let [[lhs op rhs] (tree/children node)]
                                        (list op lhs rhs))
                :binary-op (symbol (tree/only-child node))
                :variable-expr (eval/eval node {})
                :value (eval/eval node {}))))]
    (walk/prewalk f node)))

(defmethod eval/eval :constrained-by-expr
  [node env]
  (let [model (eval/eval-child node env :model-expr)
        event (-> (tree/get-node node :constrain-event-expr)
                  (event->sexpr))
        opts {:operation? seq?
              :operator first
              :operands rest
              :variable? keyword?}]
    (gpm/constrain model event opts)))
