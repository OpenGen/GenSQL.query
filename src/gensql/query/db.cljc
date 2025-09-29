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
  (let [object-reader {'object (fn [[class-name _ string-repr]]
                                 (case class-name
                                   java.time.LocalDate
                                   (java.time.LocalDate/parse string-repr),
                                   java.time.LocalDateTime
                                   (java.time.LocalDateTime/parse string-repr),
                                   string-repr))}
        sppl-readers {'gensql.gpm.spe/SPE (dynaload/dynaload 'gensql.gpm.sppl/read-string)
                      'inferenceql.gpm.spe/SPE (dynaload/dynaload 'gensql.gpm.sppl/read-string)} ; for backwards-compatibility
        readers (merge object-reader gpm/readers sppl-readers)]
    (edn/read-string {:readers readers
                      :default (fn [tag value]
                                 (println "Found unknown tag:" tag "with value:" value)
                                 {:tag tag :value value})}
                     s)))

#?(:clj (defn slurp
          [x]
          (read-string (clojure/slurp x))))

(defn empty
  []
  {})

(defn get-table
  [db k]
  (get-in db [:gensql/tables k]))

(defn safe-get-table
  [db k]
  (if-let [table (get-table db k)]
    table
    (throw (ex-info "Table does not exist"
                    {::anomalies/category ::anomalies/incorrect}))))

(defn get-model
  [db k]
  (get-in db [:gensql/models k]))

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
    (assoc-in db [:gensql/tables table-name] table)))

(defn with-model
  "Adds a model with key `k` to the database. Turns k into a string."
  [db k model]
  (let [model-name (name k)]                                ; TODO: keep using `name`?
    (assoc-in db [:gensql/models model-name] model)))

(defn env
  "A map used for SCI lookup of models/relations."
  ;; FIXME: Check for, or handle, name collisions between models and tables.
  [db]
  (merge (:gensql/tables db)
         (:gensql/models db)))
