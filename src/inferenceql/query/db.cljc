(ns inferenceql.query.db
  "This file defines functions for the creation, interrogation, and manipulation
  of InferenceQL databases."
  (:refer-clojure :exclude [empty read-string slurp])
  (:require #?(:clj [clojure.core :as clojure])
            [borkdude.dynaload :as dynaload]
            [clojure.edn :as edn]
            [cognitect.anomalies :as-alias anomalies]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.string :as query.string]))

(defn read-string
  [s]
  (let [sppl-readers {'inferenceql.gpm.spe/SPE (dynaload/dynaload 'inferenceql.gpm.sppl/read-string)}
        readers (merge gpm/readers sppl-readers)]
    (edn/read-string {:readers readers} s)))

#?(:clj (defn slurp
          [x]
          (read-string (clojure/slurp x))))

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
  "Adds a table with key `k` to the database. Turns k into a symbol."
  [db k table]
  (let [sym (query.string/safe-symbol (name k))]
    (assoc-in db [:iql/tables sym] table)))

(defn with-model
  "Adds a model with key `k` to the database. Turns k into a symbol."
  [db k model]
  (let [sym (query.string/safe-symbol (name k))]
    (assoc-in db [:iql/models sym] model)))

(defn env
  [db]
  (merge (:iql/tables db)
         (:iql/models db)))
