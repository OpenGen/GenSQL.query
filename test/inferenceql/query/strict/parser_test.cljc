(ns inferenceql.query.strict.parser-test
  (:require [clojure.test :refer [are deftest testing]]
            [inferenceql.query.strict.parser :as parser]
            [instaparse.core :as insta]))

(deftest parse
  (are [succeeds s] (= succeeds (not (insta/failure? (parser/parse s))))
    false ""
    false "SELECT *"
    false "data1 JOIN data2"

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
