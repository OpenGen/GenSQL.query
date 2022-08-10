(ns inferenceql.query.error
  (:require [cognitect.anomalies :as-alias anomalies]
            [instaparse.core :as instaparse]))

(defn parse
  [parse-result]
  (let [failure (instaparse/get-failure parse-result)]
    (throw (ex-info (with-out-str (print failure))
                    {::anomalies/category ::anomalies/incorrect
                     :instaparse/failure failure}))))
