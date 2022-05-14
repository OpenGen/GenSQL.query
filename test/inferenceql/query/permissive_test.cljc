(ns inferenceql.query.permissive-test
  (:require [clojure.test :refer [are deftest]]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.permissive :as permissive]))

(deftest permissive-valid
  (are [permissive strict] (= strict (-> permissive
                                         (parser/parse :start :given-expr)
                                         (permissive/->strict)
                                         (parser/unparse)))
    "model GIVEN VAR x = 0" "model CONDITIONED BY VAR x = 0"
    "model GIVEN VAR x > 0" "model CONSTRAINED BY VAR x > 0"
    "model GIVEN VAR x = 0 AND VAR y = 0" "model CONDITIONED BY VAR x = 0 CONDITIONED BY VAR y = 0"
    "model GIVEN VAR x > 0 AND VAR y > 0" "model CONSTRAINED BY VAR x > 0 CONSTRAINED BY VAR y > 0"
    "model GIVEN VAR x = 0 AND VAR y > 0" "model CONDITIONED BY VAR x = 0 CONSTRAINED BY VAR y > 0"
    "model GIVEN VAR x > 0 AND VAR y = 0" "model CONSTRAINED BY VAR x > 0 CONDITIONED BY VAR y = 0"))
