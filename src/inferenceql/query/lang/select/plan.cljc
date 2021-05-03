(ns inferenceql.query.lang.select.plan
  "Functions for constructing and manipulating selection plans. Selection plans
  are the combination of a Datalog query and inputs to that query. For more
  information see `inferencql.query.lang.select.plan/plan`."
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [inferenceql.query.collections :as coll]
            [inferenceql.query.datalog :as datalog]
            [inferenceql.query.node :as node]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.parser.tree :as tree]))

(def eid-var '?e)
(def entity-var '?entity)

(defmulti clauses (fn [node _] (node/tag node)))

(defmethod clauses :default
  [node env]
  (let [child-nodes (tree/child-nodes node)]
    (if-not (= 1 (count child-nodes))
      (throw (ex-info "Datalog clauses for node is not defined" {:node node}))
      (clauses (first child-nodes) env))))

(defn inputize
  "Modifies the provided query plan such that all the symbols that are in the
  environment are provided as inputs."
  [query-plan env]
  (let [input-symbols (->> env
                           (set/map-invert)
                           (map (juxt key (comp datalog/variable val)))
                           (into {}))
        replaced-symbols (->> (select-keys (:query query-plan) [:find :where])
                              (tree-seq coll? seq)
                              (filter (set (keys env)))
                              (distinct))
        input-names (zipmap (keys env)
                            (map input-symbols
                                 (vals env)))]
    (-> query-plan
        (update-in [:query] #(walk/postwalk-replace input-names %))
        (update-in [:query :in] into (map input-names replaced-symbols))
        (update-in [:inputs] into (map #(coll/safe-get env %) replaced-symbols))
        (update-in [:query :where] #(walk/postwalk (fn [form]
                                                     (cond-> form
                                                       (and (coll? form)
                                                            (= 'or-join (first form)))
                                                       (datalog/add-free-variables)))
                                                   %)))))

(defn plan
  "Given a `:select-expr` node returns a query plan for the top-most query.
  Subqueries will not be considered and are handled in a different step by the
  interpreter. See `q` for details."
  [node env]
  (let [default-from-clause (parser/parse "FROM data" :start :from-clause)

        sql-select-clause (tree/get-node node :select-clause)
        sql-from-clause   (tree/get-node node :from-clause default-from-clause)
        sql-where-clause  (tree/get-node node :where-clause)

        datalog-select-clauses (clauses sql-select-clause env)
        datalog-from-clauses   (clauses sql-from-clause env)

        datalog-where-clauses  (if sql-where-clause
                                 (clauses sql-where-clause env)
                                 {})

        all-clauses (datalog/merge datalog-from-clauses ; data source comes first
                                   datalog-select-clauses
                                   datalog-where-clauses)

        inputs (get all-clauses :inputs)
        query (dissoc all-clauses :inputs)]
    {:query query
     :inputs inputs}))
