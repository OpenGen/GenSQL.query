(ns gensql.query.statement
  (:require [gensql.query.db :as db]
            [gensql.query.literal :as literal]
            [gensql.query.parser.tree :as tree]
            [gensql.query.plan :as plan]
            [gensql.query.scalar :as scalar]))

(defn statement-node?
  [node]
  (tree/match [node]
    [[:query [:statement & _] & _]] true
    :else false))

(defn drop-table
  [db id]
  (if-not (db/get-table db id)
    db
    (update db :gensql/tables dissoc id)))

(defn safe-drop-table
  [db id]
  (when (db/safe-get-table db id)
    (update db :gensql/tables dissoc id)))

(defn drop-model
  [db id]
  (if-not (db/get-model db id)
    db
    (update db :gensql/models dissoc id)))

(defn safe-drop-model
  [db id]
  (when (db/safe-get-model db id)
    (update db :gensql/models dissoc id)))

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

    [[:create-table-stmt _create _table id-node _as expr]]
    (let [id (literal/read id-node)
          plan (plan/plan expr)
          env (db/env @db)
          out (plan/eval plan env {})]
      (swap! db db/with-table id out))

    [[:create-model-stmt _create _model id-node _as expr]]
    (let [id (literal/read id-node)
          plan (scalar/plan expr)
          env (db/env @db)
          out (scalar/eval plan env {})]
      (swap! db db/with-table id out))

    [[:drop-table-stmt _drop _table id-node]]
    (let [id (literal/read id-node)]
      (swap! db safe-drop-table id))

    [[:drop-table-stmt _drop _table _if _exists id-node]]
    (let [id (literal/read id-node)]
      (swap! db drop-table id))

    [[:drop-model-stmt _drop _model id-node]]
    (let [id (literal/read id-node)]
      (swap! db safe-drop-model id))

    [[:drop-model-stmt _drop _model _if _exists id-node]]
    (let [id (literal/read id-node)]
      (swap! db drop-model id))

    [[:insert-stmt insert into id-node & rest]]
    (let [id (literal/read id-node)
          expr `[:insert-expr ~insert ~into ~id-node ~@rest]
          plan (plan/plan expr)
          env (db/env @db)
          out (plan/eval plan env {})]
      (swap! db db/with-table id out))

    [[:update-stmt update id-node & rest]]
    (let [id (literal/read id-node)
          expr `[:update-expr ~update ~id-node ~@rest]
          plan (plan/plan expr)
          env (db/env @db)
          out (plan/eval plan env {})]
      (swap! db db/with-table id out))

    [[:alter-stmt alter id-node & rest]]
    (let [id (literal/read id-node)
          expr `[:alter-expr ~alter ~id-node ~@rest]
          plan (plan/plan expr)
          env (db/env @db)
          out (plan/eval plan env {})]
      (swap! db db/with-table id out)))
  nil)
