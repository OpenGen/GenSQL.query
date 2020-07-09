(ns inferenceql.query.server
  "This file defines functions for starting a web server that provides HTTP access
  to `inferenceql.query/q`."
  (:require [clojure.string :as string]
            [muuntaja.middleware :as middleware]
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
  [data models]
  (fn handler [request]
    (let [query (request-query request)]
      (if-not query
        {:status 400
         :body {:request request}}
        (try
          {:status 200
           :body (query/q query data models)}
          (catch clojure.lang.ExceptionInfo e
            (let [ex-data (ex-data e)
                  status (if (= :cognitect.anomalies/incorrect
                                (:cognitect.anomalies/category ex-data))
                           400
                           500)]
              {:status status
               ;; We need `select-keys` here because otherwise
               ;; `:instaparse/failure` will be included, and Instaparse failure
               ;; objects do not serialize correctly.
               :body (select-keys ex-data [:cognitect.anomalies/category
                                           :cognitect.anomalies/message])})))))))

(defn app
  "Returns a Ring handler that executes queries against the provied data and
  models. Will do basic content negotiation. Supported request content types are
  text/plain, application/edn, application/json, application/transit+json,
  application/tarnsit+msgpack. For all but the first content type the query is
  expected to be provided as a string."
  [data models]
  (middleware/wrap-format (handler data models)))
