(ns inferenceql.query.lang.literals-test
  (:require [clojure.test :refer [are deftest]]
            [inferenceql.query.lang.eval :as eval]
            [inferenceql.query.lang.literals]
            [inferenceql.query.parser :as parser]))

(deftest evaluation
  (are [s tag expected] (let [actual (-> (parser/parse s :start tag)
                                         (eval/eval {}))]
                          (= expected actual))
    "true"  :simple-symbol (symbol "true")
    "false" :simple-symbol (symbol "false")
    "true"  :bool          true
    "false" :bool          false))
