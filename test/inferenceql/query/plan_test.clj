(ns inferenceql.query.plan-test
  (:require [clojure.test :refer [are deftest is]]
            [inferenceql.inference.gpm.proto :as proto]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.plan :as plan]
            [inferenceql.query.relation :as relation]))

(def dummy-model
  (reify
    proto/GPM
    (simulate [_ targets _constraints]
      (assert (= '#{x} (set targets)))
      {'x (rand)})

    proto/Variables
    (variables [_]
      '#{x})))

(deftest expr->sexpr
  (are [expr sexpr] (= sexpr (plan/node->sexpr (parser/parse expr :start :scalar-expr)))
    "x"              'x
    "not x"          '(not x)

    "x > y"          '(> x y)
    "x > y > z"      '(> (> x y) z)

    "x = 0"          '(= x 0)
    "x = y = 0"      '(= (= x y) 0)

    "x and y or z"   '(or (and x y) z)
    "x or y and z"   '(or x (and y z))
    "x and (y or z)" '(and x (or y z))
    "(x or y) and z" '(and (or x y) z)

    "x * y + z"      '(+ (* x y) z)
    "x + y * z"      '(+ x (* y z))
    "x * (y + z)"    '(* x (+ y z))
    "(x + y) * z"    '(* (+ x y) z)))


(comment

 (plan/plan (parser/parse "select * from generate * under model;"))

 (parser/parse "from generate * under model" :start :from-clause)
 (parser/parse "from generate x, y under model" :start :from-clause)

 (require '[inferenceql.inference.gpm.proto :as proto])
 (require '[inferenceql.query.relation :as relation])

 (plan/plan (parser/parse "GENERATE * UNDER model"))
 (plan/plan (parser/parse "SELECT x + 1 FROM GENERATE * UNDER model"))

 (plan/plan (parser/parse "data"))

 (set! *print-length* 10)

 ,)
