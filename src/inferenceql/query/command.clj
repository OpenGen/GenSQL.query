(ns inferenceql.query.command
  (:require [inferenceql.query.db :as db]
            [inferenceql.query.io :as io]
            [inferenceql.query.literal :as literal]
            [inferenceql.query.strict.parser :as parser]
            [inferenceql.query.parser.tree :as tree]))

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
