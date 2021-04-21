(ns inferenceql.query.gpm.subset-test
  (:require [clojure.test :refer [are deftest testing]]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.inference.gpm.proto :as gpm.proto]
            [inferenceql.query.gpm.subset :as subset]))

(deftest subset-gpm
  (testing "variables"
    (let [model (reify gpm.proto/Variables
                  (variables [_]
                    #{:x :y}))]
      (are [vars] (= vars (gpm/variables (subset/subset-gpm model vars)))
        #{:x}
        #{:y}
        #{:x :y}))))
