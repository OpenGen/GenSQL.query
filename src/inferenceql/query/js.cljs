(ns inferenceql.query.js
  (:require [cljs-bean.core :as bean]
            [inferenceql.query :as query]))

(defn ^:export query
  "Like `inferenceql.query/q`, but accepts and returns JavaScript values."
  [query data models]
  (let [data (bean/->clj data)
        models (bean/bean models)]
    (-> (query/q query data models)
        (clj->js))))
