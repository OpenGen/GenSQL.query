(ns inferenceql.query.lang.select.where
  "Functions for evaluating the WHERE clause in a SELECT expression."
  (:require [inferenceql.query.datalog :as datalog]
            [inferenceql.query.lang.eval :as eval]
            [inferenceql.query.lang.select.plan :as plan]
            [inferenceql.query.parser.tree :as tree]))

(defmethod plan/clauses :where-clause
  [node env]
  (->> (tree/child-nodes node)
       (map #(plan/clauses % env))
       (apply datalog/merge)))

(defmethod plan/clauses :presence-condition
  [node env]
  (let [{[sym] :find :as selection-clauses} (plan/clauses (tree/get-node node :selection) env)]
    (datalog/merge (dissoc selection-clauses :find :keys)
                   {:where `[[(not= ~sym :iql/no-value)]]})))

(defmethod plan/clauses :absence-condition
  [node env]
  (let [{[sym] :find :as selection-clauses} (plan/clauses (tree/get-node node :selection) env)]
    (datalog/merge (dissoc selection-clauses :find :keys)
                   {:where `[[(= ~sym :iql/no-value)]]})))

(defmethod plan/clauses :and-condition
  [node env]
  (let [child-clauses (->> (tree/child-nodes node)
                           (mapv #(plan/clauses % env)))]
    (apply datalog/merge child-clauses)))

(defmethod plan/clauses :equality-condition
  [node env]
  (let [{[variable] :find :as selection-clauses} (plan/clauses (tree/get-node node :selection) env)
        value (eval/eval-child node env :value)]
    (datalog/merge (dissoc selection-clauses :find :keys)
                   {:where `[[(= ~variable ~value)]]})))

(defmethod plan/clauses :or-condition
  [node env]
  (let [andify (fn [subclauses]
                 (if (= 1 (count subclauses))
                   (first subclauses)
                   `(~'and ~@subclauses)))
        subclauses (map #(plan/clauses % env)
                        (tree/child-nodes node))]
    (assert (every? #(= [:where] (keys %)) subclauses))
    (let [where-subclauses (->> subclauses
                                (map :where)
                                (map andify))]
      {:where [`(~'or-join [~plan/eid-var] ~@where-subclauses)]})))

(defmethod plan/clauses :predicate-condition
  [node env]
  (let [lhs-node (tree/get-node node 0)
        {[sym] :find :as selection-clauses} (plan/clauses lhs-node env)
        predicate (eval/eval-child node env :predicate-expr)
        value     (eval/eval-child node env :value)]
    (datalog/merge (dissoc selection-clauses :find :keys)
                   {:where `[[(not= ~sym :iql/no-value)]
                             [(~predicate ~sym ~value)]]})))

(defmethod eval/eval :predicate-expr
  [node _]
  (symbol #?(:clj "clojure.core"
             :cljs "cljs.core")
          (tree/only-child node)))
