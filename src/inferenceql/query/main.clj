(ns inferenceql.query.main
  (:refer-clojure :exclude [print])
  (:require [clojure.data.csv :as csv]
            [clojure.edn :as edn]
            [clojure.main :as main]
            [clojure.pprint :as pprint]
            [clojure.repl :as repl]
            [clojure.tools.cli :as cli]
            [instaparse.core :as insta]
            [inferenceql.query :as query]
            [inferenceql.inference.gpm :as gpm]))

(def cli-options
  [["-d" "--data DATA" "data CSV path"]
   ["-m" "--model MODEL" "model EDN path"]
   ["-h" "--help"]])

(defn slurp-model
  "Opens a reader on x, reads its contents, parses its contents into EDN
  specification, and from that specification creates a multimixture model. See
  `clojure.java.io/reader` for a complete list of supported arguments."
  [x]
  (-> (slurp x) (edn/read-string) (gpm/Multimixture)))

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
  (cond (insta/failure? result) (clojure.core/print result)
        (instance? Exception result) (repl/pst result)
        :else (let [columns (:iql/columns (meta result))]
                (pprint/print-table
                 (map name columns)
                 (for [row result]
                   (reduce-kv (fn [m k v]
                                (assoc m (name k) v))
                              {}
                              row))))))

(defn repl
  "Launches an interactive InferenceQL REPL (read-eval-print loop)."
  [data models]
  (let [repl-options [:prompt #(clojure.core/print "iql> ")
                      :read (fn [request-prompt request-exit]
                              (case (main/skip-whitespace *in*)
                                :line-start request-prompt
                                :stream-end request-exit
                                (read-line)))
                      :eval #(try (query/q % data models)
                                  (catch Exception e
                                    e))
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
        {:keys [data model help]} options]
    (cond (seq errors)
          (doseq [error errors]
            (errorln error))

          (or help (nil? data) (nil? model))
          (errorln summary)

          :else
          (let [data (slurp-csv data)
                model (slurp-model model)]
            (repl data {:model model})))))
