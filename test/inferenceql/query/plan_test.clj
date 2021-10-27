(ns inferenceql.query.plan-test
  (:refer-clojure :exclude [alter eval])
  (:require [clojure.test :refer [are deftest is]]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.plan :as plan]
            [inferenceql.query.relation :as relation]))

(defn eval
  [query env]
  (-> query
      (parser/parse)
      (plan/plan)
      (plan/eval env)))

(deftest values
  (are [query rel] (= rel (eval query {}))
    "(x) VALUES (0)"               '[{x 0}]
    "(x) VALUES (0), (1)"          '[{x 0} {x 1}]
    "(x, y) VALUES (0, 1)"         '[{x 0 y 1}]
    "(x, y) VALUES (0, 1), (2, 3)" '[{x 0 y 1} {x 2 y 3}]))

(deftest insert-into
  (are [query in out] (= out (eval query {'data in}))
    "INSERT INTO data (x) VALUES (0)"           '[]          '[{x 0}]
    "INSERT INTO data (x) VALUES (0), (1), (2)" '[]          '[{x 0} {x 1}{x 2}]
    "INSERT INTO data (x) VALUES (1)"           '[{x 0}]     '[{x 0} {x 1}]
    "INSERT INTO data (x, y) VALUES (1, 1)"     '[{x 0 y 0}] '[{x 0 y 0} {x 1 y 1}]))

(deftest update-where
  (are [query in out] (= out (eval query {'data in}))
    "UPDATE data SET x=1"           '[]                    '[]
    "UPDATE data SET x=1"           '[{x 1}]               '[{x 1}]
    "UPDATE data SET x=1"           '[{x 0}]               '[{x 1}]
    "UPDATE data SET x=1"           '[{x 0} {x 0}]         '[{x 1} {x 1}]
    "UPDATE data SET x=1 WHERE x=0" '[{x 0}]               '[{x 1}]
    "UPDATE data SET x=1 WHERE x=0" '[{x 0} {x 2}]         '[{x 1} {x 2}]
    "UPDATE data SET x=1 WHERE x=0" '[{x 2} {x 0}]         '[{x 2} {x 1}]
    "UPDATE data SET x=1 WHERE y=0" '[{x 0 y 0}]           '[{x 1 y 0}]
    "UPDATE data SET x=1 WHERE y=0" '[{x 0 y 0} {x 2 y 2}] '[{x 1 y 0} {x 2 y 2}]
    "UPDATE data SET x=1 WHERE y=0" '[{x 2 y 2} {x 0 y 0}] '[{x 2 y 2} {x 1 y 0}]))

(deftest alter
  (let [data (relation/relation '[{x 0} {x 1} {x 2}] '[x])]
    (is (= data (eval "ALTER data ADD y" {'data data}))))

  (are [query in out] (= out (-> (eval query {'data (relation/relation [] in)})
                                 (relation/attributes)))
    "ALTER data ADD x" '[]    '[x]
    "ALTER data ADD x" '[x]   '[x]
    "ALTER data ADD y" '[x]   '[x y]
    "ALTER data ADD z" '[x y] '[x y z]))
