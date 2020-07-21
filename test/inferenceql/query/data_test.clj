(ns inferenceql.query.data-test
  (:require [clojure.test :refer [deftest is]]
            [inferenceql.query.data :as data]))

(deftest binary-coercer
  (let [coerce (data/value-coercer :binary)]
    (is (= true (coerce "true")))
    (is (= false (coerce "false")))))

(deftest gaussian-coercer
  (let [coerce (data/value-coercer :gaussian)]
    (is (= 1982.0 (coerce "1982")))))

(deftest categorical-coercer
  (let [coerce (data/value-coercer :categorical)]
    (is (= "April" (coerce "April")))))

(deftest map-coercion
  (let [coerce (data/row-coercer {:programmer :binary
                                  :month :categorical
                                  :year :gaussian})]
    (is (= {:programmer true
            :month "April"
            :year 1982.0}
           (coerce {:programmer "true"
                    :month "April"
                    :year "1982.0"})))))
