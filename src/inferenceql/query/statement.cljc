(ns inferenceql.query.statement
  (:require [inferenceql.query.db :as db]
            [inferenceql.query.literal :as literal]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.plan :as plan]
            [inferenceql.query.scalar :as scalar]))

(defn statement-node?
  [node]
  (tree/match [node]
    [[:query [:statement & _] & _]] true
    :else false))

(defn drop-table
  [db sym]
  (if-not (db/get-table db sym)
    db
    (update db :iql/tables dissoc sym)))

(defn safe-drop-table
  [db sym]
  (when (db/safe-get-table db sym)
    (update db :iql/tables dissoc sym)))

(defn drop-model
  [db sym]
  (if-not (db/get-model db sym)
    db
    (update db :iql/models dissoc sym)))

(defn safe-drop-model
  [db sym]
  (when (db/safe-get-model db sym)
    (update db :iql/models dissoc sym)))

(defn execute
  [node db]
  (tree/match [node]
    [[:query child]]
    (execute child db)

    [[:query child _semicolon]]
    (execute child db)

    [[:statement-bang child _bang]]
    (execute child db)

    [[:statement child]]
    (execute child db)

    [[:create-table-stmt _create _table sym-node _as expr]]
    (let [sym (literal/read sym-node)
          plan (plan/plan expr)
          env (db/env @db)
          out (plan/eval plan env {})]
      (swap! db db/with-table sym out))

    [[:create-model-stmt _create _model sym-node _as expr]]
    (let [sym (literal/read sym-node)
          plan (scalar/plan expr)
          env (db/env @db)
          out (scalar/eval plan env {})]
      (swap! db db/with-table sym out))

    [[:drop-table-stmt _drop _table sym-node]]
    (let [sym (literal/read sym-node)]
      (swap! db safe-drop-table sym))

    [[:drop-table-stmt _drop _table _if _exists sym-node]]
    (let [sym (literal/read sym-node)]
      (swap! db drop-table sym))

    [[:drop-model-stmt _drop _model sym-node]]
    (let [sym (literal/read sym-node)]
      (swap! db safe-drop-model sym))

    [[:drop-model-stmt _drop _model _if _exists sym-node]]
    (let [sym (literal/read sym-node)]
      (swap! db drop-model sym))

    [[:insert-stmt insert into sym-node & rest]]
    (let [sym (literal/read sym-node)
          expr `[:insert-expr ~insert ~into ~sym-node ~@rest]
          plan (plan/plan expr)
          env (db/env @db)
          out (plan/eval plan env {})]
      (swap! db db/with-table sym out))

    [[:update-stmt update sym-node & rest]]
    (let [sym (literal/read sym-node)
          expr `[:update-expr ~update ~sym-node ~@rest]
          plan (plan/plan expr)
          env (db/env @db)
          out (plan/eval plan env {})]
      (swap! db db/with-table sym out))

    [[:alter-stmt alter sym-node & rest]]
    (let [sym (literal/read sym-node)
          expr `[:alter-expr ~alter ~sym-node ~@rest]
          plan (plan/plan expr)
          env (db/env @db)
          out (plan/eval plan env {})]
      (swap! db db/with-table sym out)))
  nil)

(comment

  (require '[inferenceql.query.parser :as parser] :reload)

  (statement-node? (parser/parse "drop table data"))
  (statement-node? (parser/parse "drop table data!"))

  (parser/parse "drop table data")
  (parser/parse "insert into data (x) values (0)!")

  ,)
