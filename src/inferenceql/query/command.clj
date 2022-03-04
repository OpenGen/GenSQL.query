(ns inferenceql.query.command
  (:require [inferenceql.query.db :as db]
            [inferenceql.query.io :as io]
            [inferenceql.query.literal :as literal]
            [inferenceql.query.parser :as parser]
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

    [:import-command _import [:path path] sym-node]
    (let [sym (literal/read sym-node)
          table (io/slurp-csv (parser/unparse path))]
      (swap! db db/with-table sym table))

    [:export-command _import [:path path] sym-node]
    (let [sym (literal/read sym-node)
          table (db/safe-get-table @db sym)]
      (io/spit-csv table path))))

(comment

  table

  (require '[inferenceql.query.parser] :reload)

  (execute (parser/parse))

  (parser/parse ".quit")
  (parser/parse ".import ~/Desktop/zane/ignored.csv ignored")

  ,)
