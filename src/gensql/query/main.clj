(ns gensql.query.main
  (:refer-clojure :exclude [eval print])
  (:require [clojure.core :as clojure]
            [clojure.data.csv :as csv]
            [clojure.main :as main]
            [clojure.pprint :as pprint]
            [clojure.repl :as repl]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]
            [gensql.query.cli :as query.cli]
            [gensql.query.db :as db]
            [gensql.query.io :as io]
            [gensql.query.log :as query.log]
            [gensql.query.permissive :as permissive]
            [gensql.query.relation :as relation]
            [gensql.query.strict :as strict])
  (:gen-class))

(def output-formats #{"csv" "table"})
(def langs #{"permissive" "strict"})


(def cli-options
  [["-t" "--table NAME=PATH" "table CSV name and path"
    :multi true
    :default []
    :update-fn conj
    :validate [query.cli/parse-named-pair "Must be of the form: NAME=PATH"]]
   ["-m" "--model NAME=PATH" "model EDN name and path"
    :multi true
    :default []
    :update-fn conj
    :validate [query.cli/parse-named-pair "Must be of the form: NAME=PATH"]]
   ["-d" "--db PATH" "database path"]
   ["-l" "--lang LANG" "query language"
    :default "strict"
    :validate [langs (str "Must be one of: " (string/join ", " langs))]]
   ["-e" "--eval STRING" "evaluate query in STRING"]
   ["-o" "--output FORMAT" "output format"
    :validate [output-formats (str "Must be one of: " (string/join ", " output-formats))]]
   ["-h" "--help"]])

(defn print-table
  "Prints the results of an GenSQL query to the console as a table."
  [result]
  (if (instance? Exception result)
    (query.log/print-exception result)
    (let [columns (relation/attributes result)
          header-row (map name columns)
          cells (for [row result]
                  (reduce-kv (fn [m k v]
                               (assoc m (name k) v))
                             {}
                             row))]
      (pprint/print-table header-row cells))))

(defn print-csv
  "Prints the results of an GenSQL query to the console as a CSV."
  [result]
  (if (instance? Exception result)
    (query.log/print-exception result)
    (let [v (relation/->vector result)]
      (csv/write-csv *out* v)
      (flush))))

(defn make-eval
  "Returns a function that evaluates queries in the context of a database."
  [query-fn db]
  (fn [query]
    (try (query-fn query db)
         (catch Exception e
           e))))

(defn repl
  "Launches an interactive GenSQL REPL (read-eval-print loop)."
  [query-fn db & {:keys [print] :or {print print-table}}]
  (let [wrapped-print (fn [x]
                        (when-not (nil? x)
                          (print x)))
        repl-options [:prompt #(clojure.core/print "gensql> ")
                      :read (fn [request-prompt request-exit]
                              (case (main/skip-whitespace *in*)
                                :line-start request-prompt
                                :stream-end request-exit
                                (read-line)))
                      :eval (make-eval query-fn (atom db))
                      :print wrapped-print]]
    (apply main/repl repl-options)))

(defn -main
  "Main function for the GenSQL command-line application. Intended to be run
  with clj -m. Run with -h or --help for more information."
  [& args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-options)
        {models :model, query :eval, tables :table, :keys [db help lang output]} options

        print (case output
                "table" print-table
                "csv" print-csv
                nil print-table)]
    (cond (seq errors)
          (doseq [error errors]
            (query.log/errorln error))

          help
          (query.log/errorln summary)

          :else
          (let [swap-in (fn [s]
                          (if (= "-" s)
                            *in*
                            s))
                models (-> (into {}
                                 (map query.cli/parse-named-pair)
                                 models)
                           (update-keys symbol)
                           (update-vals swap-in)
                           (update-vals io/slurp-model))
                tables (-> (into {}
                                 (map query.cli/parse-named-pair)
                                 tables)
                           (update-keys symbol)
                           (update-vals swap-in)
                           (update-vals io/slurp-csv))
                db (as-> (if db
                           (db/slurp (swap-in db))
                           (db/empty))
                       %
                     (reduce-kv db/with-table % tables)
                     (reduce-kv db/with-model % models))
                query-fn (case lang
                           "permissive" permissive/query
                           "strict" strict/query)]
            (if query
              (let [eval (make-eval query-fn (atom db))]
                (print (eval query)))
              (repl query-fn db :print print))))))
