(ns inferenceql.query
  "This file defines functions for parsing, transforming, and executing IQL-SQL
  queries."
  (:refer-clojure :exclude [eval])
  (:require [hashp.core]
            [inferenceql.query.db :as db]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.plan :as plan]
            [inferenceql.query.relation :as relation]
            [instaparse.core :as insta]
            [medley.core :as medley]))

(def default-table
  "The name used for the table provided to `q`."
  ^:prviate
  'data)

(defn q
  "Returns the result of executing a query on a set of rows. A registry
  mapping model names to model values models can be provided as an optional
  third argument."
  [query db]
  (let [node-or-failure (parser/parse query)]
    (if-not (insta/failure? node-or-failure)
      (let [plan (plan/plan node-or-failure)
            env (atom (db/env db))
            out-rel (plan/eval plan env {})
            kw-rel (map #(medley/map-keys keyword %)
                        out-rel)]
        (with-meta kw-rel
          {:iql/columns (map keyword (relation/attributes out-rel))}))
      (let [failure (insta/get-failure node-or-failure)
            ex-map {:cognitect.anomalies/category :cognitect.anomalies/incorrect
                    :instaparse/failure failure}]
        (throw (ex-info "Parsing failure" ex-map))))))


(comment

  ,)
