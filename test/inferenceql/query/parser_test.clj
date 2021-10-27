(ns inferenceql.query.parser-test
  (:require [clojure.test :refer [deftest are]]
            [inferenceql.query.parser :as parser]
            [instaparse.core :as insta]))

(deftest parse
  (require '[inferenceql.query.parser :as parser] :reload)
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
    true "UPDATE data SET x = 0, y = 1 WHERE z = 1"))
