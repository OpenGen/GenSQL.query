(ns gensql.query.perf.main
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]
            [gensql.query.cli :as query.cli]
            [gensql.query.db :as db]
            [gensql.query.io :as io]
            [gensql.query.log :as query.log]
            [gensql.query.perf :as perf]
            [gensql.query.strict :as strict])
  (:gen-class))

(def langs #{"permissive" "strict"})
(def analysis-types #{"quick-benchmark" "benchmark" "profile" "time"})

(def cli-options
  [["-t" "--table NAME=PATH" "table CSV name and path"
    :id :tables
    :multi true
    :default []
    :update-fn conj
    :validate [query.cli/parse-named-pair "Must be of the form: NAME=PATH"]]
   ["-m" "--model NAME=PATH" "model EDN name and path"
    :id :models
    :multi true
    :default []
    :update-fn conj
    :validate [query.cli/parse-named-pair "Must be of the form: NAME=PATH"]]
   ["-d" "--db PATH" "database path"
    :id :db]
   ;; Only supporting strict queries for now
   #_ ["-l" "--lang LANG" "query language (strict or permissive)"
    :default "strict"
    :validate [langs (str "Must be one of: " (string/join ", " langs))]]
   ["-e" "--eval STRING" "evaluate query in STRING"
    :id :query]
   [nil "--type TYPE" "performance analysis type (quick-benchmark, benchmark, time, or profile)"
    :default "quick-benchmark"
    :validate [analysis-types (str "Must be one of: " (string/join ", " analysis-types))]]
   [nil "--target-duration DURATION" "target total time in ms spent benchmarking for type time"
    :default 10000
    :parse-fn parse-long]
   [nil  "--dry-run" "Will set up, load, and run each query once, but will not benchmark/profile"
    :id :dry-run?
    :default false]
   ["-o" "--output-file FILE" "File to write results to."]
   [nil "--[no-]overwrite" "Overwrite an existing output file"
    :id :overwrite?
    :default true]
   ["-h" "--help"]

   ;; Synthetic options
   [nil "--synthetic" "Whether to generate synthetic data/models instead of loading from files"
    :id :synthetic?
    :default false]
   [nil "--num-rows NUM-ROWS" "The number of rows the synthetic table should have"
    :default (long 1e4)
    :parse-fn parse-long]
   [nil "--num-columns NUM-COLUMNS" "The number of columns the synthetic table should have"
    :default (long 1e2)
    :parse-fn parse-long]
   [nil "--p-categorical P-CATEGORICAL" "The proportion (not probability) of coumns that are categorical. The rest are Gaussian."
    :default 0.5
    :parse-fn parse-double]
   [nil "--num-views NUM-VIEWS" "The number of views in the synthetic model."
    :default 10
    :parse-fn parse-long]
   [nil "--num-clusters-per-view NUM-CLUSTERS-PER-VIEW" "The number of clusters per view in the synthetic model."
    :default 5
    :parse-fn parse-long]
   [nil "--m M" "The m Gaussian hyperparameter"
    :default 1
    :parse-fn parse-double]
   [nil "--r R" "The r Gaussian hyperparameter"
    :default 2
    :parse-fn parse-double]
   [nil "--s S" "The s Gaussian hyperparameter"
    :default 3
    :parse-fn parse-double]
   [nil "--nu NU" "The nu Gaussian hyperparameter"
    :default 4
    :parse-fn parse-double]
   [nil "--categorical-alpha CATEGORICAL-ALPHA" "The alpha CRP concentration parameter for categorical columns in the model."
    :default 0.01
    :parse-fn parse-double
    :validate [pos? "Categorical alpha must be > 0"]]
   [nil "--local-alpha LOCAL-ALPHA" "The alpha CRP concentration parameter for clustering rows in the model."
    :default 0.01
    :parse-fn parse-double
    :validate [pos? "Local alphas must be > 0"]]
   [nil "--global-alpha GLOBAL-ALPHA" "The alpha CRP concentration parameter for column grouping in the model."
    :default 0.01
    :parse-fn parse-double
    :validate [pos? "Global alpha must be > 0"]]])

(defn load-db
  "Read in data and models from local CSV/EDN files."
  [models tables db]
  (let [swap-in (fn [s]
                  (if (= "-" s)
                    *in*
                    s))
        models (-> (into {}
                         (map query.cli/parse-named-pair)
                         models)
                   (update-keys str)
                   (update-vals swap-in)
                   (update-vals io/slurp-model))
        tables (-> (into {}
                         (map query.cli/parse-named-pair)
                         tables)
                   (update-keys str)
                   (update-vals swap-in)
                   (update-vals io/slurp-csv))
        db (as-> (if db
                   (db/slurp (swap-in db))
                   (db/empty))
                 %
                 (reduce-kv db/with-table % tables)
                 (reduce-kv db/with-model % models))]
    db))

(defn ^:private synthetic-options-str
  [{:keys [num-rows num-columns p-categorical num-views
           num-clusters-per-view m r s nu categorical-alpha
           local-alpha global-alpha type]}]
  (str "Perf run benchmark-type=" type
       " num-rows=" num-rows
       " num-columns=" num-columns
       " num-views=" num-views
       " num-clusters-per-view=" num-clusters-per-view
       " p-categorical=" p-categorical
       " m=" m
       " r=" r
       " s=" s
       " nu=" nu
       " categorical-alpha=" categorical-alpha
       " local-alpha=" local-alpha
       " global-alpha=" global-alpha))

(defn ^:private save-results
  [results output-file options]
  (if (and (seqable? results)
           (seq results))
    (let [results (if (map? results)
                    (assoc results :cli-options options)
                    results)]
      (spit (or output-file
                (str (synthetic-options-str options) ".edn"))
            (pr-str results)))
    (println "No results returned. Nothing to save.")))

(defn -main
  [& args]
  (let [start (System/currentTimeMillis)
        {:keys [options errors summary]} (cli/parse-opts args cli-options)
        {:keys [db help synthetic? models tables type query dry-run? output-file overwrite? target-duration]} options
        output-file (or output-file
                        (and synthetic? (str (synthetic-options-str options) ".edn")))]

    (cond (seq errors)
          (doseq [error errors]
            (query.log/errorln error))

          help
          (println summary)

          (and synthetic? (or (seq models) (seq tables)))
          (do
            (query.log/errorln "Cannot currently combine synthetic and real tables/models.")
            (println summary))

          (and output-file (not overwrite?) (fs/exists? output-file))
          (query.log/errorln (str "Cannot overwrite '" output-file "'. File already exists and --no-overwrite was set."))

          :else
          (let [perf-fn (if dry-run?
                          (fn [db queries]
                            (run! #(do
                                     (println (str "Running query once: " %))
                                     (strict/q % db))
                                  (vals queries)))
                          (case type
                            "quick-benchmark" #(perf/benchmark %1 %2 {:quick? true})
                            "benchmark" #(perf/benchmark %1 %2 {:quick? false})
                            "time" #(perf/time+ %1 %2 {:duration target-duration})
                            "profile" #(perf/profile %1 %2)))]
            (if synthetic?
              (let [test-suite-opts (select-keys options
                                                 [:num-rows :num-columns :p-categorical :num-views
                                                  :num-clusters-per-view :m :r :s :nu :categorical-alpha
                                                  :local-alpha :global-alpha])
                    _ (println "Synthetic suite options:" (pr-str test-suite-opts))
                    {:keys [db queries]} (perf/synthetic-test-suite test-suite-opts)
                    results (perf-fn db queries)]
                (save-results results output-file options))

              (let [db (load-db models tables db)
                    queries {:query query}
                    results (perf-fn db queries)]
                (when output-file
                  (save-results results output-file options))))))

    (println (str "\nTotal execution time: " (/ (- (System/currentTimeMillis) start) 1000.0) " seconds."))))
