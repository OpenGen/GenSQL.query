(ns inferenceql.query.statement
  (:refer-clojure :exclude [eval])
  (:require [inferenceql.query.db :as db]
            [inferenceql.query.literal :as literal]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.plan :as plan]))

(defn statement-node?
  [node]
  (tree/match [node]
    [[:query [:statement & _] & _]] true
    [[:query [:relation-expr & _] & _]] false
    false))

(defn drop-table
  [db sym]
  (if-not (db/get-table db sym)
    db
    (update db :iql/tables dissoc sym)))

(defn safe-drop-table
  [db sym]
  (when (db/safe-get-table db sym)
    (update db :iql/tables dissoc sym)))

(defn eval
  [node]
  (tree/match [node]
    [[:query child]]
    (eval child)

    [[:query child _semicolon]]
    (eval child)

    [[:statement-bang child _semicolon]]
    (eval child)

    [[:statement child]]
    (eval child)

    [[:drop-stmt _drop _table sym-node]]
    (let [sym (literal/read sym-node)]
      #(safe-drop-table % sym))

    [[:drop-stmt _drop _table _if _exists sym-node]]
    (let [sym (literal/read sym-node)]
      #(drop-table % sym))

    [[:insert-stmt insert into sym-node & rest]]
    (let [sym (literal/read sym-node)
          expr `[:insert-expr ~insert ~into ~sym-node ~@rest]
          plan (plan/plan expr)]
      (fn [db]
        (let [env (db/env db)
              out (plan/eval plan (atom env) {})]
          (db/with-table db sym out))))

    [[:update-stmt update sym-node & rest]]
    (let [sym (literal/read sym-node)
          expr `[:update-expr ~update ~sym-node ~@rest]
          plan (plan/plan expr)]
      (fn [db]
        (let [env (db/env db)
              out (plan/eval plan (atom env) {})]
          (db/with-table db sym out))))

    [[:alter-stmt alter sym-node & rest]]
    (let [sym (literal/read sym-node)
          expr `[:alter-expr ~alter ~sym-node ~@rest]
          plan (plan/plan expr)]
      (fn [db]
        (let [env (db/env db)
              out (plan/eval plan (atom env) {})]
          (db/with-table db sym out))))))

(comment

  (require '[inferenceql.query.parser :as parser] :reload)

  (statement-node? (parser/parse "drop table data"))
  (statement-node? (parser/parse "drop table data!"))

  (parser/parse "drop table data")
  (parser/parse "insert into data (x) values (0)!")

  ,)
