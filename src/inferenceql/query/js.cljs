(ns inferenceql.query.js
  (:require [clojure.edn :as edn]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.inference.gpm.multimixture :as mmix]
            [inferenceql.query.strict :as query]))

(defn read-db
  [s]
  (let [readers (assoc gpm/readers
                       'inferenceql.inference.gpm.multimixture.Multimixture
                       mmix/map->Multimixture)]
    (edn/read-string {:readers readers} s)))

(defn ^:export query
  "Like `inferenceql.query.permissive/q`, but accepts and returns JavaScript values."
  [query s]
  (let [db (read-db s)]
    (-> (query/q query db)
        (clj->js))))
