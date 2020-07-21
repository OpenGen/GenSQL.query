(ns inferenceql.query.server.main
  "This file defines a `-main` function for starting the server defined in
  `inferenceql.query.server`."
  (:require [clojure.tools.cli :as cli]
            [ring.adapter.jetty :as jetty]
            [inferenceql.query.data :as data]
            [inferenceql.query.main :as main]
            [inferenceql.query.server :as server]))

(def cli-options
  [["-d" "--data DATA" "data CSV path"]
   ["-m" "--model MODEL" "model EDN path"]
   ["-h" "--help"]])

(defn -main
  "Main function for the InferenceQL query web server. Intended to be run with clj
  -m. Run with -h or --help for more information."
  [& args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-options)
        {url :model, :keys [data help]} options]
    (cond (seq errors)
          (doseq [error errors]
            (main/errorln error))

          (or help (nil? data) (nil? url))
          (main/errorln summary)

          :else
          (let [model (main/model url)
                row-coercer (data/row-coercer (get-in model [:model :vars]))
                models {:model model}
                data (mapv row-coercer (main/slurp-csv data))
                app (server/app data models)]
            (jetty/run-jetty app {:port 3000 :join? false})))))
