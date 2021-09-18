(ns inferenceql.query.plan-test
  (:require [clojure.test :refer [are deftest is]]
            [inferenceql.inference.gpm.proto :as proto]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.plan :as plan]
            [inferenceql.query.relation :as relation]))

(deftest q
  (are [input query result] (= result (plan/q query {'data input}))
    '[{x 0} {x 1} {x 2}] "SELECT * FROM data;"                       '[{x 0} {x 1} {x 2}]
    '[{x 0} {x 1} {x 2}] "SELECT * FROM data WHERE x < 2;"           '[{x 0} {x 1}]
    '[{x 0} {x 1} {x 2}] "SELECT * FROM data WHERE x < 1 OR x > 1;"  '[{x 0} {x 2}]
    '[{x 0} {x 1} {x 2}] "SELECT * FROM data WHERE x > 0 AND x < 2;" '[{x 1}]

    '[{x 0 y 1} {x 1 y 0}] "SELECT * FROM (SELECT * FROM data);" '[{x 0 y 1} {x 1 y 0}]
    '[{x 0 y 1} {x 1 y 0}] "SELECT * FROM (SELECT x FROM data);" '[{x 0} {x 1}]
    '[{x 0 y 1} {x 1 y 0}] "SELECT x FROM (SELECT * FROM data);" '[{x 0} {x 1}]))


(deftest infinite
  (let [model (reify
                proto/GPM
                (simulate [_ targets _constraints]
                  (zipmap targets (repeatedly rand)))

                proto/Variables
                (variables [_]
                  '#{x}))
        result (plan/q "SELECT * FROM GENERATE * UNDER model;" {'model model})]
    (is (relation/relation? result))))

(deftest expr->sexpr
  (are [expr sexpr] (= sexpr (plan/scalar-expr->sexpr (parser/parse expr :start :scalar-expr)))
    "x"              'x
    "not x"          '(not x)

    "x > y"          '(> x y)
    "x > y > z"      '(> x y z)

    "x = 0"          '(= x 0)
    "x = y = 0"      '(= x y 0)

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

 (parser/parse "SELECT * FROM GENERATE * UNDER model;")

 (set! *print-length* 10)

 ,)
