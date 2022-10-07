(ns inferenceql.query.plan-test
  (:refer-clojure :exclude [alter count distinct eval])
  (:require [clojure.test :refer [are deftest is testing]]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.strict.parser :as parser]
            [inferenceql.query.permissive.parser :as parser-perm]
            [inferenceql.query.permissive :as perm]
            [inferenceql.query.plan :as plan]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.tuple :as tuple]))

(defn eval
  [query env]
  (-> query
      (parser/parse)
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



(def prob_xy '[
               {probability 0.01 x "no"  y "no" }
               {probability 0.05 x "no"  y "yes"}
               {probability 0.05 x "yes" y "no" }
               {probability 0.89 x "yes" y "yes"}
               ])




(select-keys {'probability 0.01 'x "no"  'y "no" } #{'y, 'x})
;XXX: next:
; 1. check if categorical simulate cares about the weights being normalized or ; not.
; 2. Check: should this be memoized?

; this seems to work...
(def table prob_xy)
(def condit {'y "no"})
(filter #(= condition (select-keys % (keys condition))) table)
;... which implies, this is the right implementation -- modulo a potential need
;for normalization.
(defn condition-table
  [table conditions]
  (let [conditions (update-keys conditions symbol)
        filtered-table (filter #(= condition (select-keys % (keys condition))) table)
        norm-const (reduce #(+ ('probability %1) ('probability %2)) filtered-table)
        ]
    (map (fn [row] (update row 'probability #(/ % norm-const))) filtered-table)))

(condition-table table condit)

(defn eval2
  [query env]
  (-> query
      (perm/parse)
      (plan/plan)
      (plan/eval env {})))


(def q0 "SELECT * FROM GENERATE VAR x UNDER m LIMIT 3")
(def q00 "SELECT * FROM GENERATE x UNDER m LIMIT 3")
(def q1 "SELECT * FROM prob_xy")
(def q2 "SELECT * FROM GENERATE VAR x, VAR y ACCORDING TO PROBABILITY TABLE prob_xy LIMIT 3")
(def q3 "SELECT * FROM GENERATE VAR x ACCORDING TO PROBABILITY TABLE prob_xy LIMIT 10")
(def q4 "SELECT * FROM GENERATE x ACCORDING TO PROBABILITY TABLE prob_xy LIMIT 10")

(def q5 "SELECT * FROM GENERATE VAR x ACCORDING TO PROBABILITY TABLE prob_xy CONDITIONED BY VAR y=\"no\" LIMIT 1")
(def q6 "SELECT * FROM GENERATE VAR x ACCORDING TO PROBABILITY TABLE prob_xy CONDITIONED BY VAR y=\"no\" LIMIT 100000")
;
;(parser/parse q1)
;(parser/parse q2)
;(parser/parse q3)
;(perm/parse q4)
;(perm/parse q00)
;(eval q3 {'m model 'prob_xy prob_xy})
;(eval2 q4 {'m model 'prob_xy prob_xy})
;(eval2 q00 {'m model 'prob_xy prob_xy})

;(time (def out1 (doall (eval q5 {'m model 'prob_xy prob_xy}))))
;(time (def out2 (doall (eval q6 {'m model 'prob_xy prob_xy}))))
;
;(parser/parse q5)
;(filter #(= {'x "yes"} %) (eval q5 {'m model 'prob_xy prob_xy}))
;(/ 0.05 0.06)
;(clojure.core/count (filter #(= {'x "yes"} %) (eval q5 {'m model 'prob_xy prob_xy})))






