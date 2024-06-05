(ns gensql.query.perf
  (:require [clj-async-profiler.core :as prof]
            [clojure.java.browse :as browse]
            [clojure.string :as str]
            [criterium.core :as crit]
            [gensql.query.perf.synthetic :as synthetic]
            [gensql.query.perf.util :as util]
            [gensql.query.strict :as strict]
            [medley.core :as medley]))

(def ^:dynamic *default-limit* 1000)

(defn- str-listify
  [x]
  (if (string? x) x (str/join ", " x)))

(defn conditioned-prob-density
  [{:keys [model table prob-density-evt conditioned-density-evt]}]
  (str "SELECT PROBABILITY DENSITY OF " prob-density-evt
       " UNDER " model
       " CONDITIONED BY " conditioned-density-evt
       " FROM " table))

(defn generate
  [{:keys [col-vars model limit]
    :or {limit *default-limit*}}]
  (str "SELECT * FROM"
       " GENERATE " (str-listify col-vars)
       " UNDER " model
       " LIMIT " limit))

(defn generate-conditioned-by
  [{:keys [col-vars model conditioned-density-evt limit]
    :or {limit *default-limit*}}]
  (str "SELECT * FROM"
       " GENERATE " (str-listify col-vars)
       " UNDER " model
       " CONDITIONED BY " conditioned-density-evt
       " LIMIT " limit))

(defn generative-join
  [{:keys [model table gen-join-density-evt]}]
  (str table " GENERATIVE JOIN " model
       " CONDITIONED BY " gen-join-density-evt))

(defn approximate-mutual-info
  [{:keys [model categorical-col-1 categorical-col-2]}]
  ;; NB: Why did we choose approximate MI between two column variables? (2024-3-2)
  ;;
  ;; Ulli: MI between two columns is most generally applicable (unlike MI that
  ;; involves a binary event, where we first have to define an event). We’ll
  ;; eventually replace the underlying inference machinery to deal with mutual
  ;; information between two categorical column variables with a deterministic
  ;; computation. Currently, the Monte Carlo approximation is the only way to
  ;; do that.

  ;; MD: Part of the goal is to compare the approx MC MI with the analytic MI,
  ;; once the analytic MI computation is in place. 2024-05-23

  ;; See https://www.notion.so/chi-mit/What-and-how-to-benchmark-187b69403577409ab291787b0d095154?pvs=4#91a25358ea934886b007dd303760e49b
  (str "SELECT APPROXIMATE MUTUAL INFORMATION OF VAR " categorical-col-1
       " WITH VAR " categorical-col-2
       " UNDER " model
       " FROM (dummy) VALUES (0)"))

(def default-query-fns {:conditioned-prob-density conditioned-prob-density
                        :generate generate
                        :generate-conditioned-by generate-conditioned-by
                        :generative-join generative-join
                        :approximate-mutual-info approximate-mutual-info})

(defn default-queries
  "Returns a map of the default perf queries, reusing the arguments
  between queries.

  Parameters
  - model: the name of the model
  - table: the name of the table
  - col-vars: column variable(s) - either a string or a coll of strings
  - prob-density-evt: a probability density event (e.g., \"VAR foo = true\")
  - conditioned-density-evt: a density event to condition by
  - gen-join-density-evt: a generative join density event (e.g., \"VAR foo = foo\")
  - categorical-col-1: the name of the first categorical column for MI
  - categorical-col-2: the name of the second categorical column for MI

  Example usage:
    (default-queries {:model \"mod\"
                      :table \"tbl\"
                      :col-vars [\"VAR Anticipated_Lifetime\" \"VAR Period_minutes\"]
                      :conditioned-density-evt \"VAR Power_watts = 1000\"
                      :categorical-col-1 \"Class_of_Orbit\"
                      :categorical-col-2 \"Launch_Site\"
                      :gen-join-density-evt \"VAR Purpose = Purpose\"
                      :prob-density-evt \"VAR Launch_Mass_kg = 2000\"})"
  [arg-m]
  (update-vals default-query-fns (fn [f] (f arg-m))))

(defn time+
  "Uses a variant of time+ from https://clojure-goes-fast.com/kb/benchmarking/time-plus/.

  Parameters
  - db: the database to run the queries on
  - queries: Either (1) a map of queries to run, keys are names, vals are GenSQL
    strict queries, or (2) a single GenSQL strict query in a string
  - opts - a map of options

  opts map keys:
  - warmup-iterations: the number of iterations to run to warmup queries (default: 5)
  - duration: the duration in ms to aim for (default: 10000)
  - print? - whether to print out info (default: true)
  - return-results? - whether to return the results of the benchmarked fn - can lead to OOM errors if true (default: false)

  Within the time limit, runs as many iterations as it can, then reports the
  mean time taken, the mean bytes allocated, the number of iterations, and the
  total time taken.

  NB: This is preferable when criterium's warmup period is unstable. Criterium
  attempts to wait for the Hotspot compiler to stabilize during warmup. Part of
  the criteria is two executions of the function being benchmarked with no
  changes in the reported JIT status. Unfortunately, not much JIT info is
  available at runtime to programs, so it's hard to know if JIT changes are
  relevant. Even worse, for very long run times (e.g., 30s+), the odds of a JIT
  change increase. This can result in extremely long warmup periods, on the
  order of 20+ minutes in some cases."
  ([db queries]
   (time+ db queries {}))
  ([db queries {:keys [duration warmup-iterations print? return-results?]
                :or   {duration          10000
                       warmup-iterations 5
                       print?            true
                       return-results? false}
                :as   opts}]
   (let [queries (if (string? queries) {:query queries} queries)
         time-fn (fn time-fn
                   [query]
                   (when print?
                     (println "\nTiming query:" query)
                     (println "Duration goal:" duration "ms")
                     (println "Running" warmup-iterations "warmup iterations"))

                   (dotimes [_ warmup-iterations]
                     (dorun (strict/q query db)))

                   (when print? (println "Timing main iterations"))

                   (util/time+ duration
                     ;; dorun/doall forces all lazy results to be realized during timing
                     (if return-results?
                       (doall (strict/q query db))
                       (dorun (strict/q query db)))))]

     (update-vals queries time-fn))))

(defn benchmark
  "Benchmarks the query(ies). Prints out the summary results. Returns the
  Criterium results as a map.

  Parameters
  - db: the database to run the queries on
  - queries: Either (1) a map of queries to run, keys are names, vals are GenSQL
    strict queries, or (2) a single GenSQL strict query in a string
  - opts - a map of options

  Options map
  - quick? - whether to use quick-benchmark or benchmark (default: true)
  - return-results? - whether to return the results of the benchmarked fn - can lead to OOM errors if true (default: false)
  - print? - whether to print out benchmark info (default: true)"
  ([db queries]
   (benchmark db queries {}))
  ([db queries {:keys [return-results? print? quick?]
                :or {return-results? false
                     print?          true
                     quick?          true}
                :as opts}]
   (let [queries (if (string? queries) {:query queries} queries)
         criterium-bench (if quick? crit/quick-benchmark* crit/benchmark*)
         bmark (fn bmark
                 [query]
                 (when print?
                   (if quick?
                     (println "\nQuick-benchmarking query:" query)
                     (println "\nBenchmarking query:" query)))
                 (criterium-bench
                   ;; doall forces all lazy results to be realized during benchmarking
                   (fn []
                     (if return-results?
                       (doall (strict/q query db))
                       (dorun (strict/q query db))))
                   {:warmup-jit-period 0}))
         bmark-results (update-vals queries bmark)]
     (dorun (medley/map-kv (fn [query-k bmark-result]
                             (when print?
                               (println ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
                               (println (str "Results for \"" (get queries query-k) "\":")))
                             (crit/report-result bmark-result))
                           bmark-results))
     bmark-results)))

(defn profile
  "Starts clj-async-profiler, then runs the query(ies) with the given
  arguments. Open up a browser window to see the flame graphs.

  Parameters
  - db: the database to run the queries on
  - queries: Either (1) a map of queries to run, keys are names, vals are GenSQL
    strict queries, or (2) a single GenSQL strict query in a string
  - opts - a map of options

  opts map keys:
  - num-iterations: the number of iterations to run the queries (default: 100)"
  ([db queries]
   (profile db queries {:num-iterations 100
                        :open-browser true}))
  ([db queries {:keys [num-iterations open-browser]}]
   (let [queries (if (string? queries) {:query queries} queries)]
     (prof/profile (medley/map-vals (fn [query]
                                      (println "\nProfiling query:" query)
                                      (dotimes [_ num-iterations]
                                        (doall (strict/q query db))))
                                    queries))
     (prof/serve-ui 8080)
     (when open-browser
       (browse/browse-url "http://localhost:8080")))))

(defn ^:private synthetic-density-event
  [column-types col-format i]
  (str "VAR " (format col-format i)
       " = "
       (let [col (nth column-types i)]
         (if (= :categorical (:type col))
           (str "'" (-> col :values first) "'")
           50))))

(defn synthetic-test-suite
  "Generates a synthetic database, with models and data, and crafts
  appropriate queries to run on them"
  [opts]
  (let [db (synthetic/generate-db opts)
        column-types (:synthetic/column-types db)
        {cat-cols :categorical gauss-cols :gaussian} (group-by :type column-types)
        col-format (synthetic/numbered-format synthetic/col-prefix (count column-types))
        col-0-str (format col-format 0)

        q-opts {:table                   synthetic/*default-table-name*
                :model                   synthetic/*default-model-name*
                :col-vars                (map #(str "VAR " (:name %)) (drop 2 column-types))
                :conditioned-density-evt (synthetic-density-event column-types col-format 0)
                :categorical-col-1       (:name (nth cat-cols 0))
                :categorical-col-2       (:name (nth cat-cols 1))
                :gen-join-density-evt    (str "VAR " col-0-str " = " col-0-str)
                :prob-density-evt        (synthetic-density-event column-types col-format 1)}]
    {:db      db
     :queries (default-queries q-opts)}))


