(ns inferenceql.query.lang.select.selections
  "Functions for evaluating the selection clauses that can appear within a
  SELECT expression."
  (:require [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.collections :as coll]
            [inferenceql.query.datalog :as datalog]
            [inferenceql.query.lang.eval :as eval]
            [inferenceql.query.lang.literals]
            [inferenceql.query.lang.select.plan :as plan]
            [inferenceql.query.math :as math]
            [inferenceql.query.parser.tree :as tree]))

(def default-model :model)

(defmethod eval/eval :event-list
  [node env]
  (let [nodes-by-tag (->> (tree/children node)
                          (group-by tree/tag))
        m (->> (:map-entry-expr nodes-by-tag)
               (map #(eval/eval % env))
               (into {}))
        ks (if (:star nodes-by-tag)
             keys
             (constantly
              (->> (:column-expr nodes-by-tag)
                   (map #(eval/eval % env)))))]
    (fn [env]
      (merge m (select-keys env (ks env))))))

(defmethod plan/clauses :density-clause
  [node env]
  (let [key (or (some-> (eval/eval-child-in node env [:label-clause :name])
                        (name)
                        (symbol))
                (gensym "density"))
        target (-> (tree/get-node-in node [:of-clause :event-list])
                   (eval/eval env))
        pdf-var (datalog/variable (str key "-function"))
        pdf (fn [row]
              (let [env (-> env
                            (merge row)
                            (assoc ::row row))
                    model (if-let [model (eval/eval-child-in node env [:under-clause :model-expr])]
                            model
                            (coll/safe-get env default-model))]
                (math/exp (gpm/logpdf model (target row) {}))))
        density-var (datalog/variable key)
        pdf-clause `[(~pdf-var ~plan/entity-var) ~density-var]]
    {:find   [density-var]
     :keys   [key]
     :in     [pdf-var]
     :inputs [pdf]
     :where  [pdf-clause]}))

(defmethod plan/clauses :column-selection
  [node env]
  (let [column (eval/eval-child node env :column-expr)
        key (symbol (or (eval/eval-child-in node env [:label-clause :name])
                        column))
        variable (datalog/genvar key)]
    {:find [variable]
     :keys [key]
     :where `[[(~'get-else ~'$ ~plan/eid-var ~column :iql/no-value) ~variable]]}))

(defmethod plan/clauses :rowid-selection
  [_ _]
  {:find [plan/eid-var]
   :keys '[rowid]
   :where [[plan/eid-var :iql/type :iql.type/row]]})

(defmethod plan/clauses :value-selection
  [node env]
  (let [key (if-let [label (tree/get-node-in node [:label-clause :name])]
              (-> (eval/eval label {})
                  (symbol))
              (gensym "value"))
        binding (datalog/variable key)
        value (eval/eval-child node env :value)]
    {:find [binding]
     :keys [key]
     :in [binding]
     :inputs [value]}))
