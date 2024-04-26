(ns gensql.query.js
  (:require [clojure.edn :as edn]
            [gensql.inference.gpm :as gpm]
            [gensql.inference.gpm.multimixture :as mmix]
            [gensql.query.strict :as query]))

(defn read-db
  [s]
  (let [readers (assoc gpm/readers
                       'gensql.inference.gpm.multimixture.Multimixture
                       mmix/map->Multimixture
                       'inferenceql.inference.gpm.multimixture.Multimixture ; for backwards-compatibility
                       mmix/map->Multimixture)]
    (edn/read-string {:readers readers} s)))

(defn ^:export query
  "Like `gensql.query.permissive/q`, but accepts and returns JavaScript values."
  [query s]
  (let [db (read-db s)]
    (-> (query/q query db)
        (clj->js))))
