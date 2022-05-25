(ns inferenceql.query.permissive-test
  (:require [clojure.test :refer [are deftest]]
            [inferenceql.query.permissive :as permissive]
            [inferenceql.query.permissive.parser :as permissive.parser]
            [inferenceql.query.strict.parser :as strict.parser]
            [instaparse.core :as insta]))

(deftest given-valid
  (are [permissive strict] (= strict (-> permissive
                                         (permissive.parser/parse :start :given-expr)
                                         (permissive/->strict)
                                         (strict.parser/unparse)))
    "model GIVEN VAR x = 0" "model CONDITIONED BY VAR x = 0"
    "model GIVEN VAR x > 0" "model CONSTRAINED BY VAR x > 0"
    "model GIVEN VAR x = 0 AND VAR y = 0" "model CONDITIONED BY VAR x = 0 CONDITIONED BY VAR y = 0"
    "model GIVEN VAR x > 0 AND VAR y > 0" "model CONSTRAINED BY VAR x > 0 CONSTRAINED BY VAR y > 0"
    "model GIVEN VAR x = 0 AND VAR y > 0" "model CONDITIONED BY VAR x = 0 CONSTRAINED BY VAR y > 0"
    "model GIVEN VAR x > 0 AND VAR y = 0" "model CONSTRAINED BY VAR x > 0 CONDITIONED BY VAR y = 0"))

(deftest given-invalid
  (are [s] (insta/failure? (permissive.parser/parse s :start :given-expr))
    "model GIVEN VAR x = 0 OR VAR y = 0"
    "model GIVEN VAR x = 0 OR VAR y > 0"
    "model GIVEN VAR x > 0 OR VAR y = 0"))

(deftest probability-valid
  (are [permissive strict] (= strict (-> permissive
                                         (permissive.parser/parse :start :probability-expr)
                                         (permissive/->strict)
                                         (strict.parser/unparse)))
    "PROBABILITY OF VAR x > 0 UNDER model"
    "PROBABILITY OF VAR x > 0 UNDER model"

    "PROBABILITY OF VAR x = 0 UNDER model"
    "PROBABILITY DENSITY OF VAR x = 0 UNDER model"

    "PROBABILITY OF VAR x > 0, VAR y > 0 UNDER model"
    "PROBABILITY OF VAR x > 0 AND VAR y > 0 UNDER model"

    "PROBABILITY OF VAR x = 0, VAR y = 0 UNDER model"
    "PROBABILITY DENSITY OF VAR x = 0 AND VAR y = 0 UNDER model"

    "PROBABILITY OF VAR x > 0, VAR y = 0 UNDER model"
    "PROBABILITY OF VAR x > 0 AND VAR y = 0 UNDER model"

    "PROBABILITY OF VAR x = 0, VAR y > 0 UNDER model"
    "PROBABILITY OF VAR x = 0 AND VAR y > 0 UNDER model"))
