(ns inferenceql.query.error
  (:require [instaparse.core :as instaparse]))

(defn parse
  [parse-result]
  (let [failure (instaparse/get-failure parse-result)
        ex-map {:cognitect.anomalies/category :cognitect.anomalies/incorrect
                :instaparse/failure failure}]
    (throw (ex-info "Parsing failure" ex-map))))
