(ns inferenceql.query.lang.constrain
  (:require [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.lang.eval :as eval]
            [inferenceql.query.lang.condition]
            [inferenceql.query.parser.tree :as tree]))

(defn event->sexpr
  "Takes a :distribution-event-expr node and returns a s-expression with the
  same structure. For instance, if the node was parsed from the string \"x=3\"
  the value returned will be '(= x 3). If the event would be a no-op nil is
  returned instead."
  [node env]
  (let [event->sexpr #(event->sexpr % env)]
    (if-not (tree/branch? node)
      node
      (case (tree/tag node)
        :distribution-event-expr (event->sexpr (tree/only-child-node node))
        :distribution-event-conjunction-expr (when-let [children (keep event->sexpr (tree/child-nodes node))]
                                               (cons 'and children))
        :distribution-event-disjunction-expr (when-let [children (keep event->sexpr (tree/child-nodes node))]
                                               (cons 'or children))
        :distribution-event-binary-relation-expr (let [[lhs op rhs] (map event->sexpr (tree/children node))]
                                                   (when rhs
                                                     (list op lhs rhs)))
        :distribution-event-binary-op (symbol (tree/only-child node))
        :event-variable-expr (let [[[variable value]] (seq (eval/eval node env))]
                               (when-not (contains?  #{nil :iql/no-value} value)
                                 (list '= (symbol variable) value)))
        :variable-expr (symbol (eval/eval node {}))
        (eval/eval node env)))))

(defmethod eval/eval :constrained-by-expr
  [node env]
  (let [model (eval/eval-child node env :model-expr)
        event (-> (tree/get-node node :distribution-event-expr)
                  (event->sexpr env))
        opts {:operation? seq?
              :operator first
              :operands rest
              :variable? symbol?}]
    (cond-> model
      event (gpm/constrain event opts))))
