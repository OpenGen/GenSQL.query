(ns inferenceql.query.io-test
  (:require [clojure.test :refer [are deftest is]]
            [clojure.java.io :as java.io]
            [inferenceql.query.io :as io]))

(defn slurp-string
  [s]
  (-> s
      (char-array)
      (java.io/reader)
      (io/slurp-csv)))

(deftest slurp-csv
  (are [s x] (= x (slurp-string s))
    "x"
    []

    "x\na"
    '[{x "a"}]

    "x\na\nb"
    '[{x "a"}
      {x "b"}]

    "x,y"
    []

    "x,y\na,b"
    '[{x "a" y "b"}]

    "x,y\na,b\nc,d"
    '[{x "a" y "b"}
      {x "c" y "d"}]

    ))

(deftest slurp-csv-integer
  (are [s x] (= x (slurp-string s))
    "int\n0\n1\n-1\n23456"
    '[{int 0}
      {int 1}
      {int -1}
      {int 23456}]))

(deftest slurp-csv-float
  (are [s x] (= x (slurp-string s))
    "float\n0.0\n1.2\n-1.2\n3.45"
    '[{float 0.0}
      {float 1.2}
      {float -1.2}
      {float 3.45}]))

(deftest slurp-csv-duplicate-column
  (is (thrown? Exception
        (slurp-string "x,x\n0,1"))))
