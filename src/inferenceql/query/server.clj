(ns inferenceql.query.server
  "This file defines functions for starting a web server that provides HTTP access
  to `inferenceql.query/q`."
  (:require [clojure.string :as string]
            [muuntaja.middleware :as middleware]
            [ring.middleware.cors :as cors]
            [ring.util.request :as request]
            [inferenceql.query :as query]))

(defn- request-query
  "Returns the query within the given Ring request map. See `app` for details."
  [request]
  (or (:body-params request)
      (when (string/starts-with? (request/content-type request)
                                 "text/plain")
        (string/trim (request/body-string request)))))

(defn- handler
  "Returns a Ring handler that executes queries against the provied data and
  models. Assumes that basic content negotiation has already taken place."
  [tables models]
  (fn handler [request]
    (let [query (request-query request)]
      (if-not query
        {:status 400
         :body {:request request}}
        (try
          (let [result (query/q query tables models)]
            {:status 200
             :body {:result result
                    :metadata (meta result)}})
          (catch clojure.lang.ExceptionInfo e
            (let [ex-data (ex-data e)
                  status (if (= :cognitect.anomalies/incorrect
                                (:cognitect.anomalies/category ex-data))
                           400
                           500)]
              {:status status
               :body (update ex-data :instaparse/failure pr-str)})))))))

(defn app
  "Returns a Ring handler that executes queries against the provied tables and
  models. Will do basic content negotiation. Supported request content types are
  text/plain, application/edn, application/json, application/transit+json,
  application/tarnsit+msgpack. For all but the first content type the query is
  expected to be provided as a string."
  [tables models]
  (-> (handler tables models)
      (cors/wrap-cors :access-control-allow-origin (constantly true)
                      :access-control-allow-methods [:post])
      (middleware/wrap-format)))
