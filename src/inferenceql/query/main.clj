(ns inferenceql.query.main
  (:refer-clojure :exclude [eval print])
  (:require [clojure.data.csv :as csv]
            [clojure.main :as main]
            [clojure.pprint :as pprint]
            [clojure.repl :as repl]
            [clojure.tools.cli :as cli]
            [inferenceql.query :as query]
            [inferenceql.query.data :as data]
            [inferenceql.inference.gpm :as gpm]))

(def cli-options
  [["-d" "--data DATA" "data CSV path"]
   ["-m" "--model MODEL" "model EDN path"]
   ["-e" "--eval STRING" "evaluate query in STRING"]
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
  then converts that table into a vector of maps. See `clojure.java.io/reader`
  for a complete list of supported arguments."
  [x]
  (let [data (csv/read-csv (slurp x))
        headers (map keyword (first data))
        rows (rest data)]
    (mapv #(zipmap headers %)
          rows)))

(defn print
  "Prints the results of an InferenceQL query to the console."
  [result]
  (if (instance? Exception result)
    (if-let [parse-failure (:instaparse/failure (ex-data result))]
      (clojure.core/print parse-failure)
      (repl/pst result))
    (let [columns (:iql/columns (meta result))]
      (pprint/print-table
       (map name columns)
       (for [row result]
         (reduce-kv (fn [m k v]
                      (assoc m (name k) v))
                    {}
                    row))))))

(defn eval
  "Evaluate a query and return the results."
  [query data models]
  (try (query/q query data models)
       (catch Exception e
         e)))

(defn repl
  "Launches an interactive InferenceQL REPL (read-eval-print loop)."
  [data models]
  (let [repl-options [:prompt #(clojure.core/print "iql> ")
                      :read (fn [request-prompt request-exit]
                              (case (main/skip-whitespace *in*)
                                :line-start request-prompt
                                :stream-end request-exit
                                (read-line)))
                      :eval #(eval % data models)
                      :print print]]
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
        {url :model, query :eval, :keys [data help]} options]
    (cond (seq errors)
          (doseq [error errors]
            (errorln error))

          (or help
              (nil? url)
              (and (nil? data) ; reading from stdin
                   (nil? query)))
          (errorln summary)

          :else
          (let [models {:model (model url)}
                row-coercer (data/row-coercer (get-in model [:model :vars]))
                data (mapv row-coercer (slurp-csv (or data *in*)))]
            (if query
              (print (eval query data models))
              (repl data models))))))
