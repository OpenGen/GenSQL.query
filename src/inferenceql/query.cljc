(ns inferenceql.query
  "This file defines functions for parsing, transforming, and executing IQL-SQL
  queries."
  (:refer-clojure :exclude [eval])
  (:require [hashp.core]
            #?(:clj [inferenceql.query.command :as command])
            [inferenceql.query.db :as db]
            [inferenceql.query.error :as error]
            [inferenceql.query.permissive.parser :as parser]
            [inferenceql.query.permissive :as permissive]
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
  "Returns a relation or nil."
  [s db]
  (let [node-or-failure (parser/parse s)]
    (cond (insta/failure? node-or-failure)
          (throw (error/parse node-or-failure))

          (plan/relation-node? node-or-failure)
          (let [strict-node (permissive/->strict node-or-failure)
                plan (plan/plan strict-node)
                env (db/env @db)]
            (plan/eval plan env {}))

          (statement/statement-node? node-or-failure)
          (do (statement/execute node-or-failure db)
              nil)

          #?@(:clj [(command/command-node? node-or-failure)
                    (do (command/execute node-or-failure db)
                        nil)]))))

(defn q
  "Returns the result of executing a query on a set of rows. A registry
  mapping model names to model values models can be provided as an optional
  third argument."
  [s db]
  (when-let [rel (query s (atom db))]
    (let [keywordize-keys #(medley/map-keys keyword %)
          kw-rel (map keywordize-keys rel)
          kw-attrs (map keyword (relation/attributes rel))]
      (with-meta kw-rel
        {:iql/columns kw-attrs}))))
