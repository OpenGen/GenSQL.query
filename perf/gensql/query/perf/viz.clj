(ns gensql.query.perf.viz
  (:require [babashka.fs :as fs]
           [clojure.edn :as edn]
           [clojure.data.json :as json]
           [clojure.set :as set]
           [clojure.walk :as walk]
           [criterium.core :as crit]
           [medley.core :as medley]))

(def vl5-schema "https://vega.github.io/schema/vega-lite/v5.json")

(def query-ks [:conditioned-prob-density :generate :generate-conditioned-by :generative-join :approximate-mutual-info])
(def opt-ks-of-interest [:categorical-alpha :global-alpha :local-alpha :m :nu
                         :num-clusters-per-view :num-columns :num-rows :num-views
                         :p-categorical :r :s])


(defn slurp-results
  "Slurps a coll of edn files, returns a vector of results"
  [files]
  (reduce (fn [coll perf-file]
            (conj coll
                  (->> perf-file
                       (.toFile)
                       slurp
                       edn/read-string)))
          []
          files))

(defn- vega-results
  "Helper fn to convert incoming data into \"tabular\" format expected by Vega,
  where the table is an vec/array, and each row in it is a map/Js object.

  Assumes each result is a map with :cli-options, and then one key per query"
  [results ->tuple-fn]
  (->> results
       (reduce (fn [vega-results result]
                 (let [opts (-> result :cli-options (select-keys opt-ks-of-interest))
                       q-result-map (medley/map-vals ->tuple-fn
                                                     (dissoc result :cli-options))
                       new-tuples (for [qk query-ks]
                                    (into (assoc opts :query (name qk))
                                          (qk q-result-map)))]
                   (into vega-results new-tuples)))
               [])
       (mapv walk/stringify-keys)))

(defn vega-time-results
  "Helper to convert output from `time`-based benchmarking. "
  [time-results]
  (vega-results time-results
                (fn [qresult]
                  (-> qresult
                      (dissoc :first-result)
                      (set/rename-keys {:time-per-call :mean-time-per-call
                                        :bytes-alloc-per-call :mean-bytes-alloc-per-call})))))

(defn write-vl-json
  "Writes a Vega spec to a file. (The suggested extension is .vl.json)

  Accepts an option map that has one option, :pretty-print?"
  ([output-path vega-spec]
   (write-vl-json output-path vega-spec {:pretty-print? false}))
  ([output-path vega-spec {:keys [pretty-print?] :or {pretty-print? false}}]
   (if pretty-print?
     (spit (str (fs/path output-path))
           (with-out-str (json/pprint vega-spec)))
     (spit (str (fs/path output-path))
           (json/write-str vega-spec)))))

(defn marginal-vega-spec
  [results]
  {"$schema"     vl5-schema
   "title"       "Marginals across all available data"
   "description" "Bar charts w/ mean means across other params"
   "facet"       {"column" {"field" "query"
                            "type"  "nominal"}}
   "spec"        {"repeat" {"row" ["num-rows"
                                   "num-columns"
                                   "num-views"
                                   "num-clusters-per-view"]}
                  "spec"   {"layer" [{"mark" {"type" "line"}
                                      "encoding"
                                      {"x"     {"type"  "ordinal"
                                                "field" {"repeat" "row"}}
                                       "y"     {"aggregate" "mean"
                                                "title"     "Mean execution time (s)"
                                                "field"     "mean-time-per-call"
                                                "type"      "quantitative"}
                                       "color" {"field"  "query"
                                                "type"   "nominal"
                                                "legend" nil}}}

                                     {"mark" {"type"    "point"
                                              "tooltip" true}
                                      "encoding"
                                      {"x"     {"type"  "ordinal"
                                                "field" {"repeat" "row"}}
                                       "y"     {"aggregate" "mean"
                                                "title"     "Mean execution time (s)"
                                                "field"     "mean-time-per-call"
                                                "type"      "quantitative"}
                                       "color" {"field"  "query"
                                                "type"   "nominal"
                                                "legend" nil}}}]}}
   "data"        {"name"   "orig"
                  "values" results}})

(defn marginal-w-stderr-vega-spec
  "Sames as marginal-vega-spec, but with std error bars. Probably not legitimate
  stats, since we know the underlying distributions come from different sets
  of parameters and should be."
  [results]
  {"$schema"     vl5-schema
   "title"       "Marginals across all available data with stderr (doubt this is correct way to represent pooled variance, since populations are different.)"
   "description" "Bar chart with error bars representing standard error."
   "facet"       {"column" {"field" "query" "type" "nominal"}}
   "spec"
   {"repeat" {"row" ["num-rows" "num-columns" "num-views" "num-clusters-per-view"]}
    "spec"   {"facet"   {"column" {"field" "query"}}
              "resolve" {"scale" {"y" "independent"}}

              "spec"
              {"layer" [{"mark" {"type" "bar"
                                 "tooltip" true}
                         "encoding"
                         {"x"     {"type" "ordinal"
                                   "field" {"repeat" "row"}}
                          "y"     {"aggregate" "mean"
                                   "title"     "Mean execution time (s)"
                                   "field"     "mean-time-per-call"
                                   "type"      "quantitative"}
                          "color" {"field" "query"
                                   "type" "nominal"
                                   "legend" nil}}}

                        {"mark" {"type"   "errorbar"
                                 "ticks"  true
                                 "extent" "stderr"
                                 "color"  "black"
                                 "orient" "vertical"}
                         "encoding"
                         {"x" {"type" "ordinal"
                               "field" {"repeat" "row"}}
                          "y" {"scale" {"zero" false}
                               "title" "Mean execution time (s)"
                               "field" "mean-time-per-call"
                               "type"  "quantitative"}}}]}}}
   "data"        {"name"   "orig"
                  "values" results}})

(defn fixed-param-bars-spec
  "Each col is a query, each row is a parameter of interest. Other params are
  fixed, unlike the marginal-vega-spec"
  [results]
  (let [common-spec {"mark"    {"type" "bar" "tooltip" true}
                     "resolve" {"scale" {"y" "independent"}}
                     "encoding"
                     {"x"      {"type" "ordinal"}
                      "y"      {"aggregate" "mean"
                                "title"     "Mean execution time (s)"
                                "field"     "mean-time-per-call"
                                "type"      "quantitative"}
                      "color"  {"field"  "query"
                                "type"   "nominal"
                                "legend" nil}
                      "column" {"field" "query" "title" "Query"}}}]
    {"$schema"     vl5-schema,
     "title"       "Marginals across for each param, holding other params to a single value per row"
     "description" "Bar chart with error bars representing standard deviation."
     "vconcat"     [(-> common-spec
                        (assoc-in ["encoding" "x" "field"] "num-rows")
                        (assoc "transform" [{"filter" {"and" [{"field" "num-columns" "equal" 500}
                                                              {"field" "num-views" "equal" 5}
                                                              {"field" "num-clusters-per-view" "equal" 5}]}}]))
                    (-> common-spec
                        (assoc-in ["encoding" "x" "field"] "num-columns")
                        (assoc "transform" [{"filter" {"and" [{"field" "num-rows" "equal" 50}
                                                              {"field" "num-views" "equal" 5}
                                                              {"field" "num-clusters-per-view" "equal" 5}]}}]))
                    (-> common-spec
                        (assoc-in ["encoding" "x" "field"] "num-views")
                        (assoc "transform" [{"filter" {"and" [{"field" "num-rows" "equal" 10}
                                                              {"field" "num-columns" "equal" 500}
                                                              {"field" "num-clusters-per-view" "equal" 5}]}}]))
                    (-> common-spec
                        (assoc-in ["encoding" "x" "field"] "num-clusters-per-view")
                        (assoc "transform" [{"filter" {"and" [{"field" "num-rows" "equal" 200}
                                                              {"field" "num-columns" "equal" 5}
                                                              {"field" "num-views" "equal" 5}]}}]))]
     "data"    {"values" results}}))

(defn fixed-param-lines-spec
  "Each col is a query, each row is a parameter of interest. Other params are
  fixed, unlike the marginal-vega-spec"
  [results]
  (let [common-spec
        {"facet"   {"column" {"field" "query"}}
         "resolve" {"scale" {"x" "shared"
                             "y" "independent"}}
         "spec" {"encoding" {"x"     {"type" "quantitative"}
                             "y"     {"aggregate" "mean"
                                      "title"     "Mean execution time (s)"
                                      "field"     "mean-time-per-call"
                                      "type"      "quantitative"}
                             "color" {"field" "query" "type" "nominal" "legend" nil}}
                 "layer"    [{"mark" {"type" "line"}}
                             {"mark" {"type"    "point"
                                      "tooltip" true}}]}}]
    {"$schema"     vl5-schema
     "title"       "Marginals across for each param, holding other params to a single value per row"
     "description" "Bar chart with error bars representing standard deviation."
     "vconcat"     [(-> common-spec
                        (assoc-in ["spec" "encoding" "x" "field"] "num-rows")
                        (assoc-in ["spec" "encoding" "x" "title"] "# rows in table")
                        (assoc "transform" [{"filter" {"and" [{"field" "num-columns" "equal" 500}
                                                              {"field" "num-views" "equal" 5}
                                                              {"field" "num-clusters-per-view" "equal" 5}]}}]))
                    (-> common-spec
                        (assoc-in ["spec" "encoding" "x" "field"] "num-columns")
                        (assoc-in ["spec" "encoding" "x" "title"] "# columns in table")
                        (assoc "transform" [{"filter" {"and" [{"field" "num-rows" "equal" 50}
                                                              {"field" "num-views" "equal" 5}
                                                              {"field" "num-clusters-per-view" "equal" 5}]}}]))
                    (-> common-spec
                        (assoc-in ["spec" "encoding" "x" "field"] "num-views")
                        (assoc-in ["spec" "encoding" "x" "title"] "# views in model")
                        (assoc "transform" [{"filter" {"and" [{"field" "num-rows" "equal" 10}
                                                              {"field" "num-columns" "equal" 500}
                                                              {"field" "num-clusters-per-view" "equal" 5}]}}]))
                    (-> common-spec
                        (assoc-in ["spec" "encoding" "x" "field"] "num-clusters-per-view")
                        (assoc-in ["spec" "encoding" "x" "title"] "# clusters per view in model")
                        (assoc "transform" [{"filter" {"and" [{"field" "num-rows" "equal" 200}
                                                              {"field" "num-columns" "equal" 5}
                                                              {"field" "num-views" "equal" 5}]}}]))]
     "data"    {"values" results}}))

(defn pairwise-scatterplot-spec
  "Each column is a query, and each row contains every pairwise param
  combination as a 2D scatterplot histogram."
  [results]
  (let [param-pairs [["num-rows" "num-columns"]
                     ["num-rows" "num-views"]
                     ["num-rows" "num-clusters-per-view"]
                     ["num-columns" "num-views"]
                     ["num-columns" "num-clusters-per-view"]
                     ["num-views" "num-clusters-per-view"]]
        common-spec
        {"facet"   {"column" {"field" "query"}}
         "resolve" {"scale" {"x"    "shared"
                             "y"    "shared"
                             "size" "independent"}}
         "spec"    {"encoding" {"x"     {"type" "ordinal"}
                                "y"     {"type" "ordinal" "scale" {"reverse" true}}
                                "size"  {"aggregate" "mean"
                                         "title"     "Mean execution time (s)"
                                         "field"     "mean-time-per-call"
                                         "type"      "quantitative"
                                         "legend"    nil}
                                "color" {"field"  "query"
                                         "type"   "nominal"
                                         "legend" nil}}
                    "mark"     {"type"    "circle"
                                "tooltip" true}}}]
    {"$schema" vl5-schema
     "title"   "GenSQL.query benchmark histogram of param combinations and their effect on run time"
     "vconcat" (mapv (fn [[param1 param2]]
                       (-> common-spec
                           (assoc-in ["spec" "encoding" "x" "field"] param2)
                           (assoc-in ["spec" "encoding" "y" "field"] param1)))
                     param-pairs)
     "data"    {"values" results}}))
