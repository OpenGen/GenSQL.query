(ns gensql.query.command
  (:require [gensql.query.db :as db]
            [gensql.query.io :as io]
            [gensql.query.literal :as literal]
            [gensql.query.strict.parser :as parser]
            [gensql.query.parser.tree :as tree]))

(defn command-node?
  [node]
  (tree/match node
    [:query _dot [:command _command]] true
    :else false))

(defn command?
  [s]
  (command-node? (parser/parse s)))

(defn execute
  [node db]
  (tree/match node
    [:query _dot [:command command]]
    (execute command db)

    [:quit-command _quit]
    (System/exit 0)

    [:import-command _import [:path path] id-node]
    (let [id (literal/read id-node)
          table (io/slurp-csv (parser/unparse path))]
      (swap! db db/with-table id table))

    [:export-command _import [:path path] id-node]
    (let [id (literal/read id-node)
          table (db/safe-get-table @db id)]
      (io/spit-csv table path))))
