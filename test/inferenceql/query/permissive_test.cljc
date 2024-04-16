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
    "SELECT (x) FROM data"
    "SELECT (x) FROM data"

    "SELECT (PROBABILITY OF x UNDER model) FROM data"
    "SELECT (PROBABILITY DENSITY OF VAR x = x UNDER model) FROM data"

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
    "model CONSTRAINED BY VAR x > 0 CONDITIONED BY VAR y = 0"))

(deftest given-and
  (are [permissive strict] (= (strict.parser/parse strict :start :model-expr)
                              (-> permissive
                                  (permissive.parser/parse :start :model-expr)
                                  (permissive/->strict)))
    "model GIVEN x = 0 AND y = 0"
    "model CONDITIONED BY VAR x = 0 CONDITIONED BY VAR y = 0"

    "model GIVEN x > 0 AND y > 0"
    "model CONSTRAINED BY VAR x > 0 CONSTRAINED BY VAR y > 0"

    "model GIVEN x = 0 AND y > 0"
    "model CONDITIONED BY VAR x = 0 CONSTRAINED BY VAR y > 0"

    "model GIVEN x > 0 AND y = 0"
    "model CONSTRAINED BY VAR x > 0 CONDITIONED BY VAR y = 0"))


(deftest given-standalone
  (are [permissive strict] (= (strict.parser/parse strict :start :model-expr)
                              (-> permissive
                                  (permissive.parser/parse :start :model-expr)
                                  (permissive/->strict)))
    "model GIVEN x"
    "model CONDITIONED BY VAR x = x"

    "model GIVEN x, y"
    "model CONDITIONED BY VAR x = x CONDITIONED BY VAR y = y"

    "model GIVEN x, y, z"
    "model CONDITIONED BY VAR x = x CONDITIONED BY VAR y = y CONDITIONED BY VAR z = z"))

(deftest given-standalone-and
  (are [permissive strict] (= (strict.parser/parse strict :start :model-expr)
                              (-> permissive
                                  (permissive.parser/parse :start :model-expr)
                                  (permissive/->strict)))
    "model GIVEN x AND y"
    "model CONDITIONED BY VAR x = x CONDITIONED BY VAR y = y"

    "model GIVEN x AND y AND z"
    "model CONDITIONED BY VAR x = x CONDITIONED BY VAR y = y CONDITIONED BY VAR z = z"))

(deftest given-star
  (are [permissive strict] (= (strict.parser/parse strict :start :model-expr)
                              (-> permissive
                                  (permissive.parser/parse :start :model-expr)
                                  (permissive/->strict)))

    "model GIVEN *"
    "model CONDITIONED BY *"

    "model GIVEN * EXCEPT (x, y)"
    "model CONDITIONED BY * EXCEPT (x, y)"

    "model GIVEN * EXCEPT x, y"
    "model CONDITIONED BY * EXCEPT x, y"

    "model GIVEN * EXCEPT x AND y AND z"
    "model CONDITIONED BY * EXCEPT x, y, z"))

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

(deftest probability-standalone
  (are [permissive strict] (= (-> strict
                                  (strict.parser/parse :start :scalar-expr))
                              (-> permissive
                                  (permissive.parser/parse :start :scalar-expr)
                                  (permissive/->strict)))
    "PROBABILITY OF x UNDER model"
    "PROBABILITY DENSITY OF VAR x = x UNDER model"

    "PROBABILITY OF x, y UNDER model"
    "PROBABILITY DENSITY OF VAR x = x AND VAR y = y UNDER model"

    "PROBABILITY OF x, y, z UNDER model"
    "PROBABILITY DENSITY OF VAR x = x AND VAR y = y AND VAR z = z UNDER model"

    "PROBABILITY OF x AND y UNDER model"
    "PROBABILITY DENSITY OF VAR x = x AND VAR y = y UNDER model"

    "PROBABILITY OF x, y, z UNDER model"
    "PROBABILITY DENSITY OF VAR x = x AND VAR y = y AND VAR z = z UNDER model"))

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
    "GENERATE VAR x, VAR y, VAR z UNDER model"

    "GENERATE * EXCEPT (x, y) UNDER model"
    "GENERATE * EXCEPT (VAR x, VAR y) UNDER model"))

(deftest approximate-mutual-info
  (are [permissive strict] (= (strict.parser/parse strict :start :scalar-expr)
                              (-> permissive
                                  (permissive.parser/parse :start :scalar-expr)
                                  (permissive/->strict)))
    "MUTUAL INFORMATION OF x WITH y UNDER model"
    "APPROXIMATE MUTUAL INFORMATION OF VAR x WITH VAR y UNDER model"

    "MUTUAL INFORMATION OF x, y WITH z UNDER model"
    "APPROXIMATE MUTUAL INFORMATION OF VAR x, VAR y WITH VAR z UNDER model"

    "MUTUAL INFORMATION OF x WITH y, z UNDER model"
    "APPROXIMATE MUTUAL INFORMATION OF VAR x WITH VAR y, VAR z UNDER model"))

(deftest mutual-info
  (are [permissive strict] (= (strict.parser/parse strict :start :scalar-expr)
                              (-> permissive
                                  (permissive.parser/parse :start :scalar-expr)
                                  (permissive/->strict)))
    "MUTUAL INFORMATION OF x > 0 WITH y > 0 UNDER model"
    "MUTUAL INFORMATION OF VAR x > 0 WITH VAR y > 0 UNDER model"

    "MUTUAL INFORMATION OF x > 0 AND y > 0 WITH z > 0 UNDER model"
    "MUTUAL INFORMATION OF VAR x > 0 AND VAR y > 0 WITH VAR z > 0 UNDER model"

    "MUTUAL INFORMATION OF x > 0 WITH y > 0 AND z > 0 UNDER model"
    "MUTUAL INFORMATION OF VAR x > 0 WITH VAR y > 0 AND VAR z > 0 UNDER model"))
