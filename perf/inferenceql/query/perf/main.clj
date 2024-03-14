(ns inferenceql.query.perf.main
  (:require [clojure.string :as string]
            [clojure.tools.cli :as cli]
            [inferenceql.query.cli :as query.cli]
            [inferenceql.query.perf :as perf]
            [inferenceql.query.permissive :as permissive]
            [inferenceql.query.strict :as strict]))

(def langs #{"permissive" "strict"})
(def analysis-types #{"benchmark" "profile"})

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
   ["-d" "--db PATH" "database path"]
   ;; Only supporting strict queries for now
   #_ ["-l" "--lang LANG" "query language (strict or permissive)"
    :default "strict"
    :validate [langs (str "Must be one of: " (string/join ", " langs))]]
   ["-e" "--eval STRING" "evaluate query in STRING"
    :id :query]
   [nil  "--type TYPE" "performance analysis type (benchmark or profile)"
    :default "benchmark"
    :validate [analysis-types (str "Must be one of: " (string/join ", " analysis-types))]]
   ["-h" "--help"]])

(defn -main
  [& args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-options)
        {:keys [db help lang models tables type query]} options]

    (cond (seq errors)
          (doseq [error errors]
            (query.cli/errorln error))

          help
          (query.cli/errorln summary)

          :else
          (let [db (query.cli/setup-db models tables db)
                queries (if query
                          [query]
                          (perf/default-queries
                            {:model "mod"
                             :table "tbl"
                             :col-var-list ["VAR Anticipated_Lifetime" "VAR Period_minutes"]
                             :conditioned-density-evt "VAR Power_watts = 1000"
                             :categorical-col-1 "Class_of_Orbit"
                             :categorical-col-2 "Launch_Site"
                             :gen-join-density-evt "VAR Purpose = Purpose"
                             :prob-density-evt "VAR Launch_Mass_kg = 2000"}))
                #_#_
                query-fn (case lang
                           "permissive" permissive/query
                           "strict" strict/query)]

            (case type
              "benchmark" (perf/benchmark db queries)
              "profile" (perf/profile db queries))))))
