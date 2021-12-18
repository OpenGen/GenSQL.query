(ns inferenceql.query.parser-test
  (:require [clojure.test :refer [are deftest testing]]
            [inferenceql.query.parser :as parser]
            [instaparse.core :as insta]))

(deftest parse
  (are [succeeds s] (= succeeds (not (insta/failure? (parser/parse s))))
    false ""
    false "SELECT *"
    true "SELECT * FROM data"
    true "SELECT * FROM data;"
    true "UPDATE data SET x=0"
    true "UPDATE data SET x = 0"
    true "UPDATE data SET x=0, y=1"
    true "UPDATE data SET x=0"
    true "UPDATE data SET x=0 WHERE y=1"
    true "UPDATE data SET x = 0 WHERE y = 1"
    true "UPDATE data SET x=0, y=1 WHERE z=1"
    true "UPDATE data SET x = 0, y = 1 WHERE z = 1"
    true "SELECT avg(x) FROM data"))

(deftest simple-symbol-parsing
  (testing "valid"
    (are [s] (not (insta/failure? (parser/parse s :start :simple-symbol)))
      "a"
      "A"
      "a0"
      "a0a"
      "a-"
      "a-a"
      "a?"))
  (testing "invalid"
    (are [s] (insta/failure? (parser/parse s :start :simple-symbol))
      "0a")))
