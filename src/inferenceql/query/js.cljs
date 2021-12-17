(ns inferenceql.query.js
  (:require [cljs-bean.core :as bean]
            [inferenceql.query :as query]))

(defn ^:export query
  "Like `inferenceql.query/q`, but accepts and returns JavaScript values."
  [query tables models]
  (let [tables (bean/->clj tables)
        models (bean/bean models)]
    (-> (query/q query tables models)
        (clj->js))))
