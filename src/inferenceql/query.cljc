(ns inferenceql.query
  "This file defines functions for parsing, transforming, and executing IQL-SQL
  queries."
  (:refer-clojure :exclude [eval])
  (:require [hashp.core]
            [inferenceql.query.db :as db]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.plan :as plan]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.statement :as statement]
            [instaparse.core :as insta]
            [medley.core :as medley]))

(def default-table
  "The name used for the table provided to `q`."
  ^:prviate
  'data)

(defn query
  [s db]
  (let [node-or-failure (parser/parse s)]
    (cond (insta/failure? node-or-failure)
          (let [failure (insta/get-failure node-or-failure)
                ex-map {:cognitect.anomalies/category :cognitect.anomalies/incorrect
                        :instaparse/failure failure}]
            (throw (ex-info "Parsing failure" ex-map)))

          (statement/statement-node? node-or-failure)
          (let [f (statement/eval node-or-failure)
                new-db (f db)]
            {::db/db new-db})

          :else
          (let [plan (plan/plan node-or-failure)
                env (atom (db/env db))
                rel (plan/eval plan env {})]
            {::relation/relation rel :iql/db db}))))

(defn q
  "Returns the result of executing a query on a set of rows. A registry
  mapping model names to model values models can be provided as an optional
  third argument."
  [s db]
  (let [keywordize-keys #(medley/map-keys keyword %)]
    (when-let [rel (-> (query s db)
                       (::relation/relation))]
      (let [kw-rel (map keywordize-keys rel)
            kw-attrs (map keyword (relation/attributes rel))]
        (with-meta kw-rel
          {:iql/columns kw-attrs})))))

(comment

  (q "select * from data where x > 0;" '{:iql/tables {data [{:x 0} {x 1}]}})
  (q "drop table data!" '{:iql/tables {data [{:x 0} {x 1}]}})

  ,)
