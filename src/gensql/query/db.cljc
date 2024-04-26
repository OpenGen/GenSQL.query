(ns gensql.query.db
  "This file defines functions for the creation, interrogation, and manipulation
  of GenSQL databases."
  (:refer-clojure :exclude [empty read-string slurp])
  (:require #?(:clj [clojure.core :as clojure])
            [borkdude.dynaload :as dynaload]
            [clojure.edn :as edn]
            [cognitect.anomalies :as-alias anomalies]
            [gensql.inference.gpm :as gpm]))

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
  "Adds a table with key `k` to the database. Turns k into a string."
  [db k table]
  (let [table-name (name k)]                                ; TODO: keep using `name`?
    (assoc-in db [:iql/tables table-name] table)))

(defn with-model
  "Adds a model with key `k` to the database. Turns k into a string."
  [db k model]
  (let [model-name (name k)]                                ; TODO: keep using `name`?
    (assoc-in db [:iql/models model-name] model)))

(defn env
  "A map used for SCI lookup of models/relations."
  ;; FIXME: Check for, or handle, name collisions between models and tables.
  [db]
  (merge (:iql/tables db)
         (:iql/models db)))
