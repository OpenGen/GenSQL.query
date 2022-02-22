(ns inferenceql.query.command-test
  (:require [clojure.test :refer [are deftest]]
            [inferenceql.query.command :as command]))

(deftest command?-true
  (are [s] (command/command? s)
    ".quit"
    ".import /tmp/table.csv table"))

(deftest command?-false
  (are [s] (not (command/command? s))
    ".quit exit"
    ".import /tmp/table.csv "))
