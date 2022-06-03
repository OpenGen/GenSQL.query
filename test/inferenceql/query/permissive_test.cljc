(ns inferenceql.query.permissive-test
  (:require [clojure.test :refer [are deftest]]
            [inferenceql.query.permissive :as permissive]
            [inferenceql.query.permissive.parser :as permissive.parser]
            [inferenceql.query.strict.parser :as strict.parser]))

(deftest query
  (are [permissive strict] (= (strict.parser/parse strict)
                              (-> permissive
                                  (permissive.parser/parse)
                                  (permissive/->strict)))
    "SELECT PROBABILITY OF x > 0 UNDER model GIVEN y > 1 AND z > 2 FROM data"
    "SELECT PROBABILITY OF VAR x > 0 UNDER model CONSTRAINED BY VAR y > 1 CONSTRAINED BY VAR z > 2 FROM data"))

(deftest given
  (are [permissive strict] (= (strict.parser/parse strict :start :model-expr)
                              (-> permissive
                                  (permissive.parser/parse :start :model-expr)
                                  (permissive/->strict)))
    "model GIVEN x = 0"
    "model CONDITIONED BY VAR x = 0"

    "model GIVEN x > 0"
    "model CONSTRAINED BY VAR x > 0"

    "model GIVEN x = 0, y = 0"
    "model CONDITIONED BY VAR x = 0 CONDITIONED BY VAR y = 0"

    "model GIVEN x > 0, y > 0"
    "model CONSTRAINED BY VAR x > 0 CONSTRAINED BY VAR y > 0"

    "model GIVEN x = 0, y > 0"
    "model CONDITIONED BY VAR x = 0 CONSTRAINED BY VAR y > 0"

    "model GIVEN x > 0, y = 0"
    "model CONSTRAINED BY VAR x > 0 CONDITIONED BY VAR y = 0"

    "model GIVEN x = 0 AND y = 0"
    "model CONDITIONED BY VAR x = 0 CONDITIONED BY VAR y = 0"

    "model GIVEN x > 0 AND y > 0"
    "model CONSTRAINED BY VAR x > 0 CONSTRAINED BY VAR y > 0"

    "model GIVEN x = 0 AND y > 0"
    "model CONDITIONED BY VAR x = 0 CONSTRAINED BY VAR y > 0"

    "model GIVEN x > 0 AND y = 0"
    "model CONSTRAINED BY VAR x > 0 CONDITIONED BY VAR y = 0"))

(deftest probability
  (are [permissive strict] (= (-> strict
                                  (strict.parser/parse :start :scalar-expr))
                              (-> permissive
                                  (permissive.parser/parse :start :scalar-expr)
                                  (permissive/->strict)))
    "PROBABILITY OF x > 0 UNDER model"
    "PROBABILITY OF VAR x > 0 UNDER model"

    "PROBABILITY OF x = 0 UNDER model"
    "PROBABILITY DENSITY OF VAR x = 0 UNDER model"

    "PROBABILITY OF x > 0, y > 0 UNDER model"
    "PROBABILITY OF VAR x > 0 AND VAR y > 0 UNDER model"

    "PROBABILITY OF x = 0, y = 0 UNDER model"
    "PROBABILITY DENSITY OF VAR x = 0 AND VAR y = 0 UNDER model"

    "PROBABILITY OF x > 0, y = 0 UNDER model"
    "PROBABILITY OF VAR x > 0 AND VAR y = 0 UNDER model"

    "PROBABILITY OF x = 0, y > 0 UNDER model"
    "PROBABILITY OF VAR x = 0 AND VAR y > 0 UNDER model"))

(deftest generate
  (are [permissive strict] (= (strict.parser/parse strict :start :relation-expr)
                              (-> permissive
                                  (permissive.parser/parse  :start :relation-expr)
                                  (permissive/->strict)))
    "GENERATE x UNDER model"
    "GENERATE VAR x UNDER model"

    "GENERATE x, y UNDER model"
    "GENERATE VAR x, VAR y UNDER model"

    "GENERATE x, y, z UNDER model"
    "GENERATE VAR x, VAR y, VAR z UNDER model"))
