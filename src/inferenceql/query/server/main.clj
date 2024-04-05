(ns inferenceql.query.server.main
  "This file defines a `-main` function for starting the server defined in
  `inferenceql.query.server`."
  (:require [clojure.tools.cli :as cli]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.db :as db]
            [inferenceql.query.io :as io]
            [inferenceql.query.main :as main]
            [inferenceql.query.server :as server]
            [ring.adapter.jetty :as jetty]))

(def cli-options
  [["-d" "--data DATA" "data CSV path"]
   ["-m" "--model MODEL" "model EDN path"]
   ["-h" "--help"]])

(defn ^:private model
  "Attempts to coerce `x` into a model. `x` must either return a multimixture
  specification when read with `clojure.java.io/reader` or be a valid
  `inferenceql.inference.gpm/http` server. "
  [x]
  (try (io/slurp-model x)
       (catch java.io.FileNotFoundException e
         (if (re-find #"https?://" x)
           (gpm/http x)
           (throw e)))))

(defn -main
  "Main function for the GenSQL.query web server. Intended to be run with clj
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
          (let [model (model url)
                data (io/slurp-csv data)
                db (-> (db/empty)
                       (db/with-table 'data data)
                       (db/with-model 'model model))
                app (server/app db)]
            (jetty/run-jetty app {:port 3000 :join? false})))))
