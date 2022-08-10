(ns inferenceql.query.db
  "This file defines functions for the creation, interrogation, and manipulation
  of InferenceQL databases."
  (:refer-clojure :exclude [empty slurp])
  (:require #?(:clj [clojure.core :as clojure])
            #?(:clj [clojure.edn :as edn])
            [cognitect.anomalies :as-alias anomalies]))

#?(:clj (defn slurp
          [x]
          (-> (clojure/slurp x) (edn/read-string))))

(defn empty
  []
  {})

(defn get-table
  [db k]
  (get-in db [:iql/tables k]))

(defn safe-get-table
  [db k]
  (if-let [table (get-table db k)]
    table
    (throw (ex-info "Table does not exist"
                    {::anomalies/category ::anomalies/incorrect}))))

(defn get-model
  [db k]
  (get-in db [:iql/models k]))

(defn safe-get-model
  [db k]
  (if-let [model (get-model db k)]
    model
    (throw (ex-info "Model does not exist"
                    {::anomalies/category ::anomalies/incorrect}))))

(defn with-table
  [db k table]
  (let [sym (symbol (name k))]
    (assoc-in db [:iql/tables sym] table)))

(defn with-model
  [db k model]
  (let [sym (symbol (name k))]
    (assoc-in db [:iql/models sym] model)))

(defn env
  [db]
  (merge (:iql/tables db)
         (:iql/models db)))
