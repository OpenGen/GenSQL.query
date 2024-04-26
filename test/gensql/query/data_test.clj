(ns gensql.query.data-test
  (:require [clojure.test :refer [deftest is]]
            [gensql.query.data :as data]))

(deftest gaussian-coercer
  (let [coerce (data/value-coercer :numerical)]
    (is (= 1982.0 (coerce "1982")))))

(deftest categorical-coercer
  (let [coerce (data/value-coercer :nominal)]
    (is (= "April" (coerce "April")))))

(deftest map-coercion
  (let [coerce (data/row-coercer {:month :nominal
                                  :year :numerical})]
    (is (= {:month "April"
            :year 1982.0}
           (coerce {:month "April"
                    :year "1982.0"})))))
