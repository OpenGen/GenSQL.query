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

(deftest slurp-csv-categorical
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
      {x "c" y "d"}]))


(deftest slurp-csv-categorical-missing
  (are [s x] (= x (slurp-string s))
    "x,y\na,"
    '[{x "a"}]

    "x,y\n,b"
    '[{y "b"}]

    "x,y\n,b\nc,d"
    '[{y "b"}
      {x "c" y "d"}]

    "x,y\na,\nc,d"
    '[{x "a"}
      {x "c" y "d"}]

    "x,y\na,b\n,d"
    '[{x "a" y "b"}
      {y "d"}]

    "x,y\na,b\nc,"
    '[{x "a" y "b"}
      {x "c"}]))

(deftest slurp-csv-integer
  (are [s x] (= x (slurp-string s))
    "int\n0\n1\n-1\n23456"
    '[{int 0}
      {int 1}
      {int -1}
      {int 23456}]))

(deftest slurp-csv-integer-missing
  (are [s x] (= x (slurp-string s))
    "x,y\n0,"
    '[{x 0}]

    "x,y\n,1"
    '[{y 1}]

    "x,y\n,1\n2,3"
    '[{y 1}
      {x 2 y 3}]

    "x,y\n0,\n2,3"
    '[{x 0}
      {x 2 y 3}]

    "x,y\n0,1\n,3"
    '[{x 0 y 1}
      {y 3}]

    "x,y\n0,1\n2,"
    '[{x 0 y 1}
      {x 2}]))

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
