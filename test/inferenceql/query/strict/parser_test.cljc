(ns inferenceql.query.strict.parser-test
  (:require [clojure.test :refer [are deftest]]
            [inferenceql.query.strict.parser :as parser]
            [instaparse.core :as insta]))

(deftest select-valid
  (are [s] (not (insta/failure? (parser/parse s)))
    "SELECT * FROM data"
    "SELECT * FROM data;"
    "SELECT x FROM data"
    "SELECT x AS y FROM data"
    "SELECT (x) FROM data"
    "SELECT (x) AS y FROM data"
    "SELECT avg(x) FROM data"
    "SELECT * EXCEPT (foo, bar) FROM data"))

(deftest select-invalid
  (are [s] (insta/failure? (parser/parse s))
    ""
    "SELECT *"
    "SELECT (x AS y) FROM data"))

(deftest update-valid
  (are [s] (not (insta/failure? (parser/parse s)))
    "UPDATE data SET x=0"
    "UPDATE data SET x = 0"
    "UPDATE data SET x=0, y=1"
    "UPDATE data SET x=0"
    "UPDATE data SET x=0 WHERE y=1"
    "UPDATE data SET x = 0 WHERE y = 1"
    "UPDATE data SET x=0, y=1 WHERE z=1"
    "UPDATE data SET x = 0, y = 1 WHERE z = 1"
    "SELECT avg(x) FROM data"))

(deftest join-invalid
  (are [s] (insta/failure? (parser/parse s))
    "data1 JOIN data2"))

(deftest generative-join-valid
  (are [s] (not (insta/failure? (parser/parse s)))
    "data GENERATIVE JOIN model"
    "data GENERATIVE JOIN model CONDITIONED BY VAR x = 0"
    "data GENERATIVE JOIN model CONDITIONED BY *"
    "data GENERATIVE JOIN model CONDITIONED BY * EXCEPT VAR x, VAR y"
    "data GENERATIVE JOIN model CONDITIONED BY * EXCEPT (VAR x)"
    "data GENERATIVE JOIN model CONSTRAINED BY VAR x > 0"))

(deftest generate-valid
  (are [s] (not (insta/failure? (parser/parse s :start :generate-expr)))
    "GENERATE * UNDER model"
    "GENERATE VAR foo, VAR bar UNDER model"
    "GENERATE * EXCEPT (VAR foo, VAR bar) UNDER model"))

(deftest conditioned-by-valid
  (are [s] (not (insta/failure? (parser/parse s)))
    "SELECT * FROM (GENERATE * UNDER model CONDITIONED BY VAR x = x)"
    "SELECT * FROM (GENERATE * UNDER model CONDITIONED BY *)"
    "SELECT * FROM (GENERATE * UNDER model CONDITIONED BY * EXCEPT (VAR x))"
    "SELECT * FROM (GENERATE * UNDER model CONDITIONED BY * EXCEPT VAR x, VAR y)"
    "SELECT * FROM (GENERATE * UNDER model CONDITIONED BY * EXCEPT VAR x, VAR \"foo.bar\", VAR z)"))
