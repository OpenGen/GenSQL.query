(ns inferenceql.query.plan-test
  (:refer-clojure :exclude [alter count distinct eval])
  (:require [clojure.test :refer [are deftest is testing]]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.permissive :as permissive]
            [inferenceql.query.permissive.parser :as permissive.parser]
            [inferenceql.query.plan :as plan]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.strict.parser :as strict.parser]
            [inferenceql.query.tuple :as tuple]))

(deftest select-plan-permissive
  (are [s] (-> s
               (permissive.parser/parse)
               (permissive/->strict)
               (plan/plan)
               (plan/plan?))
    "SELECT * FROM data"
    "SELECT x FROM data"
    "SELECT (x) FROM data"
    "SELECT PROBABILITY OF x UNDER model FROM data"
    "SELECT (PROBABILITY OF x UNDER model) FROM data"))

(defn eval
  [query env]
  (-> query
      (strict.parser/parse)
      (plan/plan)
      (plan/eval env {})))

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

  (testing "from aliasing"
    (are [query in attrs expected] (= expected
                                      (try (-> (eval query {'data (relation/relation in :attrs attrs)})
                                               (relation/tuples)
                                               (first)
                                               (tuple/->vector)
                                               (first))
                                           (catch #?(:clj Exception :cljs :default) _
                                             :error)))
      "SELECT x FROM data AS y;" '[{x 0}] '[x] 0
      "SELECT x FROM data AS y;" '[] '[x] nil
      "SELECT x FROM data AS x;" '[{x 0}] '[x] 0
      "SELECT x FROM data AS x;" '[] '[x] nil
      "SELECT y.x FROM data AS y;" '[{x 0}] '[x] 0
      "SELECT y.y FROM data AS y;" '[{y 0}] '[x] 0)))

(deftest order
  (let [data '[{x 2}
               {x 0}
               {x 1}]]
    (are [query expected] (= expected (eval query {'data data}))
      "SELECT * FROM data ORDER BY x;"
      '[{x 0}
        {x 1}
        {x 2}]

      "SELECT * FROM data ORDER BY x ASC;"
      '[{x 0}
        {x 1}
        {x 2}]

      "SELECT * FROM data ORDER BY x DESC;"
      '[{x 2}
        {x 1}
        {x 0}])))

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

(deftest aggregation-max
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

(deftest aggregation-sum
  (are [query in out] (= out (->> (eval query {'data in})
                                  (relation/tuples)
                                  (map tuple/->vector)))
    "SELECT sum(x) FROM data"            '[]                                        '[[nil]]
    "SELECT sum(x) FROM data"            '[{x 0} {x 1}]                             '[[1]]
    "SELECT sum(x) FROM data"            '[{x 1} {x 0}]                             '[[1]]
    "SELECT sum(x) FROM data"            '[{x 0} {x 1} {x 2}]                       '[[3]]
    "SELECT sum(x) FROM data"            '[{x 0} {x 2} {x 1}]                       '[[3]]
    "SELECT sum(x) FROM data"            '[{x 2} {x 1} {x 0}]                       '[[3]]
    "SELECT sum(x), min(x) FROM data"    '[{x 0} {x 1}]                             '[[1 0]]
    "SELECT sum(x), sum(y) FROM data"    '[{x 1 y 1} {x 2 y 0}]                     '[[3 1]]
    "SELECT sum(x) FROM data GROUP BY y" '[{x 0 y 0} {x 1 y 1} {x 2 y 0} {x 3 y 1}] '[[2] [4]]))

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

(def mmix
  {:vars {:x :categorical
          :y :categorical}
   :views [[{:probability 0.75
             :parameters  {:x {"yes" 1.0 "no" 0.0}
                           :y {"yes" 1.0 "no" 0.0}}}
            {:probability 0.25
             :parameters  {:x {"yes" 0.0 "no" 1.0}
                           :y {"yes" 0.0 "no" 1.0}}}]]})

(def model (gpm/Multimixture mmix))

(deftest generate
  (testing "attributes"
    (are [query attrs] (= attrs (relation/attributes (eval query {'model model})))
      "GENERATE VAR x UNDER model" '[x]
      "GENERATE VAR x, VAR y UNDER model" '[x y]
      "GENERATE VAR y, VAR x UNDER model" '[y x]
      "GENERATE VAR x, VAR y, VAR z UNDER model" '[x y z]))
  (testing "values"
    (let [rel (eval "GENERATE VAR x UNDER model" {'model model})]
      (doseq [tup (relation/tuples (take 5 rel))]
        (is (= '[x] (keys tup)))
        (is (contains? #{"yes" "no"} (tuple/get tup 'x)))))))

(deftest join
  (let [env '{a [{x 0} {x 1}]
              b [{y 0} {y 1}]}]
    (are [query expected] (= expected (eval query env))
      "a CROSS JOIN b"
      '[{x 0 y 0}
        {x 0 y 1}
        {x 1 y 0}
        {x 1 y 1}]

      "a INNER JOIN b ON true"
      '[{x 0 y 0}
        {x 0 y 1}
        {x 1 y 0}
        {x 1 y 1}]

      "a INNER JOIN b ON true"
      '[{x 0 y 0}
        {x 0 y 1}
        {x 1 y 0}
        {x 1 y 1}]

      "a INNER JOIN b ON false"
      []

      "a INNER JOIN b ON 0 = 1"
      []

      "a JOIN b ON x = y"
      '[{x 0 y 0}
        {x 1 y 1}]

      "a INNER JOIN b ON x = y"
      '[{x 0 y 0}
        {x 1 y 1}]

      "a INNER JOIN b ON a.x = y"
      '[{x 0 y 0}
        {x 1 y 1}]

      "a INNER JOIN b ON x = b.y"
      '[{x 0 y 0}
        {x 1 y 1}]

      "a INNER JOIN b ON a.x = b.y"
      '[{x 0 y 0}
        {x 1 y 1}]

      "a INNER JOIN b ON a.x = b.y"
      '[{x 0 y 0}
        {x 1 y 1}])))
