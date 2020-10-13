(ns inferenceql.query.datalog-test
  (:refer-clojure :exclude [merge])
  (:require [clojure.core.match :as match]
            [clojure.test :as test :refer [deftest is are]]
            [datascript.core :as datascript]
            [inferenceql.query.datalog :as datalog]))

(deftest merge
  (is (= '{:find [?e ?x ?y ?e ?y ?z]
           :where [[?e :iql/type :iql.type/row]
                   [?e :x ?x]
                   [?e :y ?y]
                   [?e :z ?z]]}
         (datalog/merge '{:find [?e ?x ?y]
                          :where [[?e :iql/type :iql.type/row]
                                  [?e :x ?x]
                                  [?e :y ?y]]}
                        '{:find [?e ?y ?z]
                          :where [[?e :iql/type :iql.type/row]
                                  [?e :y ?y]
                                  [?e :z ?z]]}))))

(deftest find-clause
  (are [query expected] (= expected (datalog/find-clause query))
    '[:find ?e .
      :where [?e :db/doc "Hello world"]]
    '[?e .]

    '[:find ?e .
      :in $
      :where [?e :db/doc "Hello world"]]
    '[?e .]))

(deftest in-clause
  (are [query expected] (= expected (datalog/in-clause query))
    '[:find ?e .
      :where [?e :db/doc "Hello world"]]
    '()

    '[:find ?e .
      :in $
      :where [?e :db/doc "Hello world"]]
    '[$]))

(deftest where-clauses
  (are [query expected] (= expected (datalog/where-clauses query))
    '[:find ?e .
      :where [?e :db/doc "Hello world"]]
    '[[?e :db/doc "Hello world"]]

    '[:find ?e .
      :in $
      :where [?e :db/doc "Hello world"]]
    '[[?e :db/doc "Hello world"]]))

(deftest extension
  (let [;; Works just like an inc function expression, only the binding form is
        ;; on the left-hand side.
        weird-inc (reify datalog/Extension
                    (matches? [_ clause]
                      (match/match clause
                        [out-sym (['weird-inc in-sym] :seq)] true
                        :else false))
                    (symbols [_ clause]
                      (match/match clause
                        [out-sym (['weird-inc in-sym] :seq)]
                        {:arg-syms [in-sym] :out-syms [out-sym]}))
                    (execute [_]
                      (comp vector inc)))
        query '[:find ?age2 .
                :in $ ?color
                :where
                [?e :cat/color ?color]
                [?e :cat/age ?age]
                [?age2 (weird-inc ?age)]
                [?e :cat/name ?name]]
        db [[0 :cat/name "Henry"]
            [0 :cat/color :orange]
            [0 :cat/age 11]
            [1 :cat/name "Disco"]
            [1 :cat/color :brown]
            [1 :cat/age 13]]]
    (is (= 14 (datalog/q query [weird-inc] db :brown)))
    (is (= (datascript/q query db :brown)
           (datalog/q query [] db :brown))
        "Behaves like `datascript.core/q` if no extensions are provided.")))
