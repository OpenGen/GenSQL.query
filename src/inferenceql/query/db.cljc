(ns inferenceql.query.db
  "This file defines functions for the creation, interrogation, and manipulation
  of InferenceQL databases."
  (:refer-clojure :exclude [empty slurp])
  (:require [clojure.core :as clojure]
            [clojure.edn :as edn]))

(defn empty
  []
  {})

(defn slurp
  [x]
  (-> (clojure/slurp x) (edn/read-string)))

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
