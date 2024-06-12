(ns gensql.query.main-test
  (:refer-clojure :exclude [eval])
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gensql.query.io :as query.io]
            [gensql.query.main :as main]
            [gensql.query.relation :as relation]))

(deftest print-csv-round-trip
  (let [input (relation/relation [{"x" 0 "y" 1}])
        csv-str (with-out-str (main/print-csv input))
        output (query.io/slurp-csv (io/reader (char-array csv-str)))]
    (is (= input output))))
