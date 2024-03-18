(ns inferenceql.query.main
  (:refer-clojure :exclude [eval print])
  (:require [clojure.core :as clojure]
            [clojure.data.csv :as csv]
            [clojure.main :as main]
            [clojure.pprint :as pprint]
            [clojure.repl :as repl]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]
            [inferenceql.query.db :as db]
            [inferenceql.query.io :as io]
            [inferenceql.query.permissive :as permissive]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.strict :as strict])
  (:gen-class))

(def output-formats #{"csv" "table"})
(def langs #{"permissive" "strict"})

(defn parse-named-pair
  [s]
  (when-let [[_ name path] (re-matches #"([^=]+)=([^=]+)" s)]
    {name path}))

(def cli-options
  [["-t" "--table NAME=PATH" "table CSV name and path"
    :multi true
    :default []
    :update-fn conj
    :validate [parse-named-pair "Must be of the form: NAME=PATH"]]
   ["-m" "--model NAME=PATH" "model EDN name and path"
    :multi true
    :default []
    :update-fn conj
    :validate [parse-named-pair "Must be of the form: NAME=PATH"]]
   ["-d" "--db PATH" "database path"]
   ["-l" "--lang LANG" "query language"
    :default "strict"
    :validate [langs (str "Must be one of: " (string/join ", " langs))]]
   ["-e" "--eval STRING" "evaluate query in STRING"]
   ["-o" "--output FORMAT" "output format"
    :validate [output-formats (str "Must be one of: " (string/join ", " output-formats))]]
   ["-h" "--help"]])

(defn print-exception
  [e]
  (binding [*out* *err*
            *print-length* 10
            *print-level* 4]
    (if-let [parse-failure (:instaparse/failure (ex-data e))]
      (clojure/print parse-failure)
      (if-let [ex-message (ex-message e)]
        (clojure/println ex-message)
        (repl/pst e)))))

(defn print-table
  "Prints the results of an InferenceQL query to the console as a table."
  [result]
  (if (instance? Exception result)
    (print-exception result)
    (let [columns (relation/attributes result)
          header-row (map name columns)
          cells (for [row result]
                  (reduce-kv (fn [m k v]
                               (assoc m (name k) v))
                             {}
                             row))]
      (pprint/print-table header-row cells))))

(defn print-csv
  "Prints the results of an InferenceQL query to the console as a CSV."
  [result]
  (if (instance? Exception result)
    (print-exception result)
    (let [columns (get (meta result)
                       :iql/columns
                       (into #{} (mapcat keys) result))
          header-row (map name columns)
          cells (map (apply juxt columns) result)
          table (into [header-row] cells)]
      (csv/write-csv *out* table))))

(defn make-eval
  "Returns a function that evaluates queries in the context of a database."
  [query-fn db]
  (fn [query]
    (try (query-fn query db)
         (catch Exception e
           e))))

(defn repl
  "Launches an interactive InferenceQL REPL (read-eval-print loop)."
  [query-fn db & {:keys [print] :or {print print-table}}]
  (let [wrapped-print (fn [x]
                        (when-not (nil? x)
                          (print x)))
        repl-options [:prompt #(clojure.core/print "iql> ")
                      :read (fn [request-prompt request-exit]
                              (case (main/skip-whitespace *in*)
                                :line-start request-prompt
                                :stream-end request-exit
                                (read-line)))
                      :eval (make-eval query-fn (atom db))
                      :print wrapped-print]]
    (apply main/repl repl-options)))

(defn errorln
  "Like `clojure.core/println`, but prints to `clojure.core/*err*` instead of
  `clojure.core/*out*`."
  [& args]
  (binding [*out* *err*]
    (apply println args)))

(defn -main
  "Main function for the InferenceQL command-line application. Intended to be run
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
            (errorln error))

          help
          (errorln summary)

          :else
          (let [swap-in (fn [s]
                          (if (= "-" s)
                            *in*
                            s))
                models (-> (into {}
                                 (map parse-named-pair)
                                 models)
                           (update-keys symbol)
                           (update-vals swap-in)
                           (update-vals io/slurp-model))
                tables (-> (into {}
                                 (map parse-named-pair)
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
