(ns inferenceql.query.validation-test
  (:require [clojure.test :refer [are deftest]]
            [inferenceql.query :as query]
            [inferenceql.query.validation :as validation]))

(defn valid?
  "Returns `true` if query string `s` is valid, `false` otherwise. "
  [s]
  (validation/valid? (query/parse s)))

(deftest select-witout-limit
  (are [valid query] (= valid (valid? query))
    ;; SELECT without LIMIT
    true  "SELECT x FROM (GENERATE x UNDER model) LIMIT 10;"
    false "SELECT x FROM (GENERATE x UNDER model);"

    ;; Non-"data" FROM
    true  "SELECT x FROM data;"
    false "SELECT x FROM table;"))
