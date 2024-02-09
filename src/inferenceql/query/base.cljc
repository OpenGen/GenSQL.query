(ns inferenceql.query.base
  "Functions for issuing queries. This is not a public API.

  Contains fns shared by both strict and permissive IQL variants."
  (:refer-clojure :exclude [eval])
  (:require #?(:clj [inferenceql.query.command :as command])
            [inferenceql.query.db :as db]
            [inferenceql.query.error :as error]
            [inferenceql.query.plan :as plan]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.statement :as statement]
            [inferenceql.query.string :as q.string]
            [instaparse.core :as insta]))

(defn query
  "Issues a query against a database. Returns a relation or nil. A parsing
  function that turns a string into a parse tree must be provdied as the third
  argument.

  Params:
  - s - a query string
  - db - a database wrapped in an atom/derefable
  - parse - a parsing function"
  [s db parse]
  (let [node-or-failure (parse s)]
    (cond (insta/failure? node-or-failure)
          (throw (error/parse node-or-failure))

          (plan/relation-node? node-or-failure)
          (let [plan (plan/plan node-or-failure)
                env (db/env @db)]
            (plan/eval plan env {}))

          (statement/statement-node? node-or-failure)
          (do (statement/execute node-or-failure db)
              nil)

          #?@(:clj [(command/command-node? node-or-failure)
                    (do (command/execute node-or-failure db)
                        nil)]))))

(defn q
  "Returns the result of executing a query on a set of rows. A parsing
  function that turns a string into a parse tree must be provided as the third
  argument.

  Keywordizes the result's keys and its attributes.

  Params:
  - s - a query string
  - db - a database
  - parse - a parsing function"
  [s db parse]
  (when-let [rel (query s (atom db) parse)]
    (let [keywordize-keys #(update-keys % q.string/safe-keyword)
          kw-rel (map keywordize-keys rel)
          kw-attrs (map q.string/safe-keyword (relation/attributes rel))]
      (with-meta kw-rel
        {:iql/columns kw-attrs}))))
