(ns gensql.query.strict
  "Functions for issuing GenSQL-strict queries."
  (:require [gensql.query.base :as base]
            [gensql.query.strict.parser :as parser]))

(defn query
  "Issues a query against a database. Returns a relation or nil."
  [s db]
  (base/query s db parser/parse))

(defn q
  "Returns the result of executing a query on a set of rows."
  [s db]
  (base/q s db parser/parse))
