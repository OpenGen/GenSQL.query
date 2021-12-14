(ns inferenceql.query.plan-test
  (:refer-clojure :exclude [alter count distinct eval])
  (:require [clojure.test :refer [are deftest is testing]]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.plan :as plan]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.tuple :as tuple]))

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

(deftest select
  (testing "attributes"
    (are [query coll in out] (= out
                                (-> (eval query {'data (relation/relation coll :attrs in)})
                                    (relation/attributes)))
      "SELECT * FROM data;" '[{x 0}]     '[x]   '[x]
      "SELECT * FROM data;" '[{x 0}]     nil    '[x]
      "SELECT * FROM data;" '[{x 0 y 1}] '[x]   '[x]
      "SELECT * FROM data;" '[{x 0}]     '[x y] '[x y]
      "SELECT x FROM data;" '[{}]        '[x]   '[x]))

  (testing "name"
    (are [query coll in expected] (= expected
                                     (-> (eval query {'data (relation/relation coll :name in)})
                                         (relation/name)))
      "SELECT * FROM a;" '[{}]    'a 'a
      "SELECT * FROM a;" '[{x 0}] 'b 'a
      "SELECT * FROM b;" '[{}]    'b 'b
      "SELECT * FROM b;" '[{x 0}] 'b 'b))

  (testing "from aliasing"
    (are [query in attrs expected] (= expected
                                      (try (-> (eval query {'data (relation/relation in :attrs attrs)})
                                               (relation/tuples)
                                               (first)
                                               (tuple/->vector)
                                               (first))
                                           (catch Exception _
                                             :error)))
      "SELECT x FROM data AS y;" '[{x 0}] '[x] 0
      "SELECT x FROM data AS y;" '[] '[x] nil
      "SELECT x FROM data AS x;" '[{x 0}] '[x] 0
      "SELECT x FROM data AS x;" '[] '[x] nil
      "SELECT y.x FROM data AS y;" '[{x 0}] '[x] 0
      "SELECT y.y FROM data AS y;" '[{y 0}] '[x] 0)))

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
  (let [data (relation/relation '[{x 0} {x 1} {x 2}] :attrs '[x])]
    (is (= data (eval "ALTER data ADD y" {'data data}))))

  (are [query in out] (= out (-> (eval query {'data (relation/relation [] :attrs in)})
                                 (relation/attributes)))
    "ALTER data ADD x" '[]    '[x]
    "ALTER data ADD x" '[x]   '[x]
    "ALTER data ADD y" '[x]   '[x y]
    "ALTER data ADD z" '[x y] '[x y z]))

(deftest aggregation
  (are [query in out] (= out (->> (eval query {'data in})
                                  (relation/tuples)
                                  (map tuple/->vector)))
    "SELECT max(x) FROM data"            '[]                                        '[[nil]]
    "SELECT max(x) FROM data"            '[{x 0} {x 1}]                             '[[1]]
    "SELECT max(x) FROM data"            '[{x 1} {x 0}]                             '[[1]]
    "SELECT max(x) FROM data"            '[{x 0} {x 1} {x 2}]                       '[[2]]
    "SELECT max(x) FROM data"            '[{x 0} {x 2} {x 1}]                       '[[2]]
    "SELECT max(x) FROM data"            '[{x 2} {x 1} {x 0}]                       '[[2]]
    "SELECT max(x), min(x) FROM data"    '[{x 0} {x 1}]                             '[[1 0]]
    "SELECT max(x), max(y) FROM data"    '[{x 0 y 1} {x 1 y 0}]                     '[[1 1]]
    "SELECT max(x) FROM data GROUP BY y" '[{x 0 y 0} {x 1 y 1} {x 2 y 0} {x 3 y 1}] '[[2] [3]]))

(deftest count
  (testing "count(x)"
    (are [query in out] (= out (->> (eval query {'data in})
                                    (relation/tuples)
                                    (map tuple/->vector)
                                    (ffirst)))
      "SELECT count(x) FROM data" '[] 0
      "SELECT count(x) FROM data" '[{x 0}] 1
      "SELECT count(x) FROM data" '[{x 0} {x 1}] 2
      "SELECT count(x) FROM data" '[{x 0} {} {x 1}] 2
      "SELECT count(x) FROM data" '[{x 0} {} {x 1}] 2
      "SELECT count(x) FROM data" '[{} {x 0} {} {x 1} {}] 2))

  (testing "count(*)"
    (are [query in out] (= out (->> (eval query {'data in})
                                    (relation/tuples)
                                    (map tuple/->vector)
                                    (ffirst)))
      "SELECT count(*) FROM data" '[] 0
      "SELECT count(*) FROM data" '[{}] 1
      "SELECT count(*) FROM data" '[{} {}] 2
      "SELECT count(*) FROM data" '[{} {} {}] 3)))

(deftest distinct
  (testing "DISTINCT"
    (are [query in out] (= out (eval query {'data in}))
      "SELECT DISTINCT x FROM data" '[] '[]
      "SELECT DISTINCT x FROM data" '[{x 0}] '[{x 0}]
      "SELECT DISTINCT x FROM data" '[{x 0} {x 0}] '[{x 0}]
      "SELECT DISTINCT x FROM data" '[{x 0} {x 1}] '[{x 0} {x 1}]
      "SELECT DISTINCT x FROM data" '[{x 0} {x 1} {x 0}] '[{x 0} {x 1}]
      "SELECT DISTINCT x FROM data" '[{x 0} {x 1} {x 1} {x 0}] '[{x 0} {x 1}]))

  (testing "aggregate DISTINCT"
    (are [query in out] (= out (->> (eval query {'data in})
                                    (relation/tuples)
                                    (map tuple/->vector)
                                    (ffirst)))
      "SELECT avg(DISTINCT x) FROM data" '[{x 0}] 0.0
      "SELECT avg(DISTINCT x) FROM data" '[{x 0} {x 1}] 0.5
      "SELECT avg(DISTINCT x) FROM data" '[{x 0} {x 0} {x 1}] 0.5
      "SELECT avg(DISTINCT x) FROM data" '[{x 0} {x 1} {x 1}] 0.5)))
