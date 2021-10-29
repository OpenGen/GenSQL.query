(ns inferenceql.query.literal-test
  (:refer-clojure :exclude [read])
  (:require [clojure.test :refer [are deftest is]]
            [inferenceql.query.literal :as literal]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.relation :as relation]))

(deftest read-relation-metadata
  (let [rel (literal/read (parser/parse "(x, y) VALUES (0, 1)" :start :relation-value))]
    (is (relation/relation? rel))
    (is (= '(x y) (relation/attributes rel)))))

(deftest read-relation-value
  (are [x s] (= x (literal/read (parser/parse s :start :relation-value)))
    '({x 0})                        "(x) VALUES (0)"
    '({x 0} {x 1})                  "(x) VALUES (0), (1)"
    '({x 0, y 1})                   "(x, y) VALUES (0, 1)"
    '({x 0, y 1} {x 2, y 3})        "(x, y) VALUES (0, 1), (2, 3)"
    '({x 0})                        "(x) VALUES ... 0: (0) ..."
    '({x 0, y 1})                   "(x, y) VALUES ... 0: (0, 1) ..."
    '({} {x 1})                     "(x) VALUES ... 1: (1) ..."
    '({} {x 1, y 2})                "(x, y) VALUES ... 1: (1, 2) ..."
    '({} {x 1} {} {x 3})            "(x) VALUES ... 1: (1), 3: (3) ..."
    '({} {x 1, y 2} {} {x 3, y 4})  "(x, y) VALUES ... 1: (1, 2), 3: (3, 4) ..."))
