(ns inferenceql.query.main
  (:refer-clojure :exclude [eval print])
  (:import [tech.tablesaw.api Row]
           [tech.tablesaw.api Table])
  (:require [clojure.core :as clojure]
            [clojure.data.csv :as csv]
            [clojure.main :as main]
            [clojure.pprint :as pprint]
            [clojure.repl :as repl]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query :as query]
            [inferenceql.query.db :as db]
            [inferenceql.query.relation :as relation]
            [medley.core :as medley]))

(def output-formats #{"csv" "table"})

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
   ["-e" "--eval STRING" "evaluate query in STRING"]
   ["-o" "--output FORMAT" "output format"
    :validate [output-formats (str "Must be one of: " (string/join ", " output-formats))]]
   ["-h" "--help"]])

(defn slurp-model
  "Opens a reader on x, reads its contents, parses its contents into EDN
  specification, and from that specification creates a multimixture model. See
  `clojure.java.io/reader` for a complete list of supported arguments."
  [x]
  (-> (slurp x) (gpm/read-string)))

(defn model
  "Attempts to coerce `x` into a model. `x` must either return a multimixture
  specification when read with `clojure.java.io/reader` or be a valid
  `inferenceql.inference.gpm/http` server. "
  [x]
  (try (slurp-model x)
       (catch java.io.FileNotFoundException e
         (if (re-find #"https?://" x)
           (gpm/http x)
           (throw e)))))

(defn slurp-csv
  "Opens a reader on x, reads its contents, parses its contents as a table, and
  then converts that table into a relation. See `clojure.java.io/reader` for a
  complete list of supported arguments."
  [x]
  (let [^Table table (.csv (Table/read) (slurp x) "")
        columns (.columnNames table)
        attrs (map symbol columns)
        row (Row. table)
        coll (loop [i 0
                    rows (transient [])]
               (if (>= i (.rowCount table))
                 (persistent! rows)
                 (do (.at row i)
                     (let [row (zipmap attrs
                                       (map #(.getObject row %)
                                            columns))]
                       (recur (inc i) (conj! rows row))))))]
    (with-meta coll {:iql/columns attrs})))

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
  "Evaluate a query and return the results."
  [db]
  (fn [query]
    (try (let [{rel ::relation/relation new-db ::db/db} (query/query query @db)]
           (when new-db (reset! db new-db))
           rel)
         (catch Exception e
           e))))

(defn repl
  "Launches an interactive InferenceQL REPL (read-eval-print loop)."
  [db & {:keys [print] :or {print print-table}}]
  (let [wrapped-print (fn [x]
                        (when-not (nil? x)
                          (print x)))
        repl-options [:prompt #(clojure.core/print "iql> ")
                      :read (fn [request-prompt request-exit]
                              (case (main/skip-whitespace *in*)
                                :line-start request-prompt
                                :stream-end request-exit
                                (read-line)))
                      :eval (make-eval (atom db))
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
        {models :model, query :eval, tables :table, :keys [db help output]} options

        print (case output
                "table" print-table
                "csv" print-csv
                nil print-table)]
    (cond (seq errors)
          (doseq [error errors]
            (errorln error))

          (or help
              (and (empty? tables) ; reading from stdin
                   (nil? db)
                   (nil? query)))
          (errorln summary)

          :else
          (let [models (->> (into {}
                                  (map parse-named-pair)
                                  models)
                            (medley/map-keys symbol)
                            (medley/map-vals slurp-model))
                tables (if-not (seq tables)
                         {'data (slurp-csv *in*)}
                         (->> (into {}
                                    (map parse-named-pair)
                                    tables)
                              (medley/map-keys symbol)
                              (medley/map-vals slurp-csv)))
                db (as-> (if db
                           (db/slurp db)
                           (db/empty))
                       %
                     (reduce-kv db/with-table % tables)
                     (reduce-kv db/with-model % models))]
            (if query
              (let [eval (make-eval (atom db))]
                (print (eval query)))
              (repl db :print print))))))


(comment


  (let [data (main/slurp-csv "/Users/zane/Desktop/ignored.csv")
        model (main/slurp-model "/Users/zane/Desktop/sample.0.edn")
        db (-> (db/empty)
               (db/with-table 'data data)
               (db/with-model 'model model))]
    #_
    (parser/parse "select approximate mutual information of var Purpose with var Users under model from data limit 1;" )
    (-main "--table" "data=/Users/zane/Desktop/ignored.csv")
    @last-error

    )

  ,)
