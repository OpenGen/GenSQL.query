(ns inferenceql.query-test
  (:refer-clojure :exclude [alter])
  #?(:clj (:import [clojure.lang ExceptionInfo]))
  (:require [clojure.string :as string]
            [clojure.test :as test :refer [are deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.walk :as walk]
            [com.gfredericks.test.chuck.generators :as chuck.gen]
            [inferenceql.inference.gpm :as gpm]
            ;; [inferenceql.inference.gpm.crosscat :as xcat]
            [inferenceql.query :as query]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.plan :as plan]
            ;; [inferenceql.query.relation :as relation]
            [instaparse.core :as insta]))

(defn q
  ([query data]
   (q query data {}))
  ([query data models]
   (query/q query {:data data} models)))

(def simple-mmix
  {:vars {:x :categorical
          :y :categorical}
   :views [[{:probability 0.75
             :parameters  {:x {"yes" 1.0 "no" 0.0}
                           :y {"yes" 1.0 "no" 0.0}}}
            {:probability 0.25
             :parameters  {:x {"yes" 0.0 "no" 1.0}
                           :y {"yes" 0.0 "no" 1.0}}}]]})

(def simple-model (gpm/Multimixture simple-mmix))

;;; Generators

(defn gen-row
  "Returns a generator that will generate individual \"rows\" (maps). Arguments to
  this function are like `gen/hash-map`, but this function accepts a map instead
  of a sequence of key/value pairs."
  [columns]
  (gen/bind (chuck.gen/sub-map columns)
            #(apply gen/hash-map (mapcat identity %))))

(def string-alpha
  (gen/fmap string/join (gen/vector gen/char-alpha)))

(def gen-symbol
  (->> (gen/tuple gen/char-alpha
                  (gen/fmap string/join
                            (gen/vector (gen/frequency [[20 gen/char-alphanumeric]
                                                        [1 (gen/return \-)]]))))
       (gen/fmap #(apply str %))
       (gen/such-that #(not (string/starts-with? % "G__")))
       (gen/fmap symbol)))

(def gen-column
  (gen/fmap keyword gen-symbol))

(def gen-label
  (gen/fmap keyword gen-symbol))

(def gen-table
  "Generator for full \"tables\" (vectors of maps). Each row will have keys drawn
  from a consistent subset, and the values for each key will be drawn from a
  fixed generator."
  (let [value-generators [gen/small-integer gen/nat gen/small-integer]]
    (gen/bind (gen/map gen-column (gen/elements value-generators))
              (comp gen/vector gen-row))))

(def gen-table-col
  "Generator for a 2-tuple of table and a column for that table."
  (gen/bind (gen/such-that #(seq (mapcat keys %))
                           gen-table)
            #(gen/tuple (gen/return %)
                        (gen/elements (mapcat keys %)))))

(def gen-table-col-subset
  (gen/bind (gen/such-that #(seq (mapcat keys %))
                           gen-table)
            #(gen/tuple (gen/return %)
                        (gen/not-empty
                         (chuck.gen/subset (mapcat keys %))))))

;;; Literals

(defn parse-and-eval
  [& args]
  (-> (apply parser/parse args)
      (plan/eval-literal)))

(defspec nat-evaluation
  (prop/for-all [n gen/nat]
    (let [s (pr-str n)]
      (is (= n (parse-and-eval s :start :nat))))))

(defspec int-evaluation
  (prop/for-all [n gen/small-integer]
    (let [s (pr-str n)]
      (is (= n (parse-and-eval s :start :int))))))

(defspec symbol-evaluation
  (prop/for-all [sym gen-symbol]
    (let [s (pr-str sym)]
      (is (= sym (parse-and-eval s :start :simple-symbol))))))

(defspec symbol-genvar-relationship
  ;; This invariant is utilized by the function
  ;; `inferenceql.query/free-variables`.
  (testing "symbols can't begin with `inferenceql.query/genvar` prefix."
    (prop/for-all [s (gen/fmap #(str "G__" %) gen/string)]
      (is (insta/failure? (parser/parse s :start :simple-symbol))))))

;;; Name

(deftest simple-symbol-parsing
  (testing "valid"
    (are [s] (not (insta/failure? (parser/parse s :start :simple-symbol)))
      "a"
      "A"
      "a0"
      "a0a"
      "a-"
      "a-a"
      "a?"))
  (testing "invalid"
    (are [s] (insta/failure? (parser/parse s :start :simple-symbol))
      "0a")))

;; Float parsing is a bit different in CLJS. Among other things, whole-numbered
;; floats are printed without a decimal point. Someone should come back and try
;; to make this work in CLJS at some point.
#?(:clj (defspec float-parsing
          (prop/for-all [n (gen/double* {:infinite? false :NaN? false})]
            (let [s (pr-str n)]
              (is (== n (parse-and-eval s :start :float)))))))

;;; Query parsing success/failure

(deftest parsing-success
  (are [start query] (nil? (insta/get-failure (parser/parse query :start start)))
    :select-expr "SELECT * FROM data"))

(deftest parsing-failure
  (are [start query] (some? (insta/get-failure (parser/parse query :start start)))
    :select-expr "123abc"))

;;; Basic selection

(deftest select-basic
  (let [data [{:x 0}
              {:x 1}
              {:x 2}]]
    (testing "with source"
      (is (= data (q "SELECT x FROM data" data))))))

(defspec select-star
  (prop/for-all [table gen-table]
    (let [results (q "SELECT * FROM data" table)]
      (is (= results table)))))

(defspec select-col
  (prop/for-all [[table ks] gen-table-col-subset]
    (let [cols (->> ks (map name) (string/join ", "))
          results (q (str "SELECT " cols " FROM data") table)]
      (is (= (map #(select-keys % ks)
                  table)
             results)))))

;;; Metadata

(deftest select-response-metadata
  (are [query data in-c out-c] (= out-c
                                  (-> (q query (cond-> data
                                                 in-c (with-meta {:iql/columns in-c})))
                                      meta
                                      :iql/columns))
    "SELECT * FROM data;" [{:x 0}]      [:x]    [:x]
    "SELECT * FROM data;" [{:x 0}]      nil     [:x]
    "SELECT * FROM data;" [{:x 0 :y 1}] [:x]    [:x]
    "SELECT * FROM data;" [{:x 0}]      [:x :y] [:x :y]
    "SELECT x FROM data;" [{}]          [:x]    [:x]))

;;; Order by

(defn ascending?
  "Returns true if the provided collection is sorted in ascending order."
  [coll]
  (->> coll
       (partition 2 1)
       (every? #(<= (apply compare %) 0))))

(defn descending?
  [coll]
  (->> coll (reverse) (ascending?)))

(defspec select-order-by
  (prop/for-all [[table col] gen-table-col]
    (let [query (str "SELECT * FROM data ORDER BY " (name col))]
      (is (->> (q query table)
               (map col)
               (ascending?))))))

(defspec select-order-by-asc
  (prop/for-all [[table col] gen-table-col]
    (let [query (str "SELECT * FROM data ORDER BY " (name col) " ASC")]
      (is (->> (q query table)
               (map col)
               (ascending?))))))

(defspec select-order-by-desc
  (prop/for-all [[table col] gen-table-col]
    (let [query (str "SELECT * FROM data ORDER BY " (name col) " DESC")]
      (is (->> (q query table)
               (map col)
               (descending?))))))

;;; Limit

(def gen-table-limit
  (gen/bind gen-table
            #(gen/tuple (gen/return %)
                        (gen/choose 0 (count %)))))

(defspec select-limit
  (prop/for-all [[table n] gen-table-limit]
    (let [results (q (str "SELECT * FROM data LIMIT " n) table)]
      (is (= (take n table)
             results)))))

(deftest select-where-limit
  (is (= [{:x 2}]
         (q "SELECT * FROM data WHERE x > 0 ORDER BY x DESC LIMIT 1"
            [{:x 0}
             {:x 1}
             {:x 0}
             {:x 2}
             {:x 0}]))))

;; Conditions

(defspec conditions-not-null
  (prop/for-all [[table k] gen-table-col]
    (let [results (q (str "SELECT * FROM data WHERE " (name k) " IS NOT NULL") table)]
      (is (= (remove (comp nil? k) table)
             results)))))

(deftest conditions-null-example
  (is (= [{}]
         (q "SELECT * FROM data WHERE x IS NULL"
            (with-meta [{}] {:iql/columns [:x]})))))

(defspec conditions-null
  (prop/for-all [[table k] gen-table-col]
    (let [results (q (str "SELECT * FROM data WHERE " (name k) " IS NULL") table)]
      (is (= (filter (comp nil? k) table)
             results)))))

(deftest conditions-precdence
  (let [q #(q % [{:x 1}])]
    (is (= [{:x 1}] (q "SELECT * FROM data WHERE (x=0 AND x=1) OR x=1")))
    (is (= []       (q "SELECT * FROM data WHERE x=0 AND (x=1 OR x=1)")))
    (testing "AND has higher precedence than OR"
      (is (= [{:x 1}] (q "SELECT * FROM data WHERE x=0 AND x=1 OR x=1"))))))

(deftest conditions-or-multi-clause-subcondition
  (let [query "SELECT * FROM data WHERE x>0 OR y>0"]
    (is (= [{:x 1 :y 0}
            {:x 0 :y 1}
            {:x 1 :y 1}]
           (q query [{:x 0 :y 0}
                     {:x 1 :y 0}
                     {:x 0 :y 1}
                     {:x 1 :y 1}])))))

(deftest conditions-predicate
  (let [query "SELECT * FROM data WHERE x>0"
        table [{:x -1}
               {:x  0}
               {:x  1}]]
    (is (= [{:x 1}] (q query table)))))

;; Probabilities

(deftest density-of-missing
  (let [model (gpm/Multimixture
               {:vars {:x :categorical
                       :y :categorical}
                :views [[{:probability 0.5
                          :parameters  {:x {"yes" 1.0 "no" 0.0}
                                        :y {"yes" 1.0 "no" 0.0}}}
                         {:probability 0.5
                          :parameters  {:x {"yes" 0.0 "no" 1.0}
                                        :y {"yes" 0.0 "no" 1.0}}}]]})
        q1 (comp first vals first #(q %1 %2 %3))]
    (is (= 0.5 (q1 "SELECT (PROBABILITY DENSITY OF VAR y = \"yes\" UNDER model CONDITIONED BY VAR x = x) FROM data;"
                   (with-meta [{}] {:iql/columns [:x :y]})
                   {:model model})))))

(deftest density-of-bindings
  (let [rows [{}]
        models {:model simple-model}
        q1 (comp first vals first #(q % rows models))]
    (is (= 0.25 (q1 "SELECT (PROBABILITY DENSITY OF VAR x=\"no\"  UNDER model)                              FROM data LIMIT 1")))
    (is (= 0.75 (q1 "SELECT (PROBABILITY DENSITY OF VAR x=\"yes\" UNDER model)                              FROM data LIMIT 1")))
    (is (= 1.0  (q1 "SELECT (PROBABILITY DENSITY OF VAR x=\"yes\" UNDER model CONDITIONED BY VAR y=\"yes\") FROM data LIMIT 1")))
    (is (= 1.0  (q1 "SELECT (PROBABILITY DENSITY OF VAR x=\"no\"  UNDER model CONDITIONED BY VAR y=\"no\")  FROM data LIMIT 1")))
    (is (= 0.0  (q1 "SELECT (PROBABILITY DENSITY OF VAR x=\"yes\" UNDER model CONDITIONED BY VAR y=\"no\")  FROM data LIMIT 1")))))

(deftest density-of-rows
  (let [models {:model simple-model}
        q1 (comp first vals first #(q %1 %2 models))]
    (are [expected x] (= expected
                         (q1 "SELECT (PROBABILITY DENSITY OF VAR x = x UNDER model) FROM data"
                             [{:x x}]))
      0.25 "no"
      0.75 "yes")

    (are [expected x y] (= expected
                           (q1 "SELECT (PROBABILITY DENSITY OF VAR x = x UNDER model CONDITIONED BY VAR y = y) FROM data"
                               [{:x x :y y}]))
      1.0 "yes" "yes"
      1.0 "no"  "no"
      0.0 "yes" "no"
      0.0 "no"  "yes")))

;;; Generate

#_
(deftest generate-generates-correct-columns
  (testing "generate"
    (let [model simple-model
          q #(q % [] {:model model})]
      (testing "with star "
        (doseq [result (q "SELECT * FROM (GENERATE * UNDER model) LIMIT 10")]
          (is (= #{:x :y} (set (keys result))))))
      (testing "with a single variable"
        (doseq [result (q "SELECT * FROM (GENERATE VAR y UNDER model) LIMIT 10")]
          (is (= #{:y} (set (keys result))))))
      (testing "with multiple variables"
        (doseq [result (q "SELECT * FROM (GENERATE VAR x, VAR y UNDER model) LIMIT 10")]
          (is (= #{:x :y} (set (keys result))))))
      (testing "expressions can have a subset of columns selected from them"
        (doseq [result (q "SELECT y FROM (GENERATE VAR x, VAR y UNDER model) LIMIT 10")]
          (is (= [:y] (keys result))))))))

;;; Invalid inputs

(deftest syntax-error
  (is (thrown? ExceptionInfo (q "invalid query" [])))
  (try (q "invalid query" [])
       (catch ExceptionInfo e
         (is (= :cognitect.anomalies/incorrect (:cognitect.anomalies/category (ex-data e)))))))

;;; Unparse

(deftest unparse
  (let [query "SELECT x, y, z FROM data;"]
    (is (= query
           (-> query
               (parser/parse)
               (parser/unparse))))
    (is (= "x, y, z"
           (-> query
               (parser/parse)
               (tree/get-node-in [:relation-expr :select-expr :select-clause :select-list])
               (parser/unparse))))))

;;; Labels

(defspec column-label
  (prop/for-all [[table column] gen-table-col
                 label gen-label]
    (is (= (->> table
                (map #(select-keys % [column]))
                (map #(walk/postwalk-replace {column label} %)))
           (q (str "SELECT " (name column) " AS " (name label) " FROM data")
              table)))))

;;; Insert

(deftest insert-into
  (let [data [{:x 0}]
        result (q "SELECT * FROM (INSERT INTO data (x) VALUES (1), (2))"
                  data)]
    (is (= [{:x 0} {:x 1} {:x 2}]
           result))))

;;; Update

(deftest update-set
  (let [data [{:x 0} {:x 1} {:x 2}]
        result (q "SELECT * FROM (UPDATE data SET x=-1)"
                  data)]
    (is (= [{:x -1} {:x -1} {:x -1}]
           result))))

(deftest update-set-where
  (let [data [{:x 0} {:x 1} {:x 2}]
        result (q "SELECT * FROM (UPDATE data SET x=-1 WHERE x>0)"
                  data)]
    (is (= [{:x 0} {:x -1} {:x -1}]
           result))))

;; FIXME
#_
(deftest update-set-where-rowid
  (let [data [{:x 0} {:x 1} {:x 2}]
        result (q "SELECT * FROM (UPDATE data SET x=-1 WHERE rowid=2)"
                  data)]
    (is (= [{:x 0} {:x -1} {:x 2}]
           result))))

;;; Alter

(deftest alter
  (let [data [{:x 0}
              {:x 1}
              {:x 2}]
        result (q "SELECT * FROM (ALTER data ADD y);"
                  data)]
    (is (= data result))
    (is (= [:x :y] (:iql/columns (meta result))))))

;; With

(deftest with
  (let [data (with-meta [] {:iql/columns [:x]})
        result (q "WITH (INSERT INTO data (x) VALUES (1), (2), (3)) AS data: SELECT * FROM data;"
                  data)]
    (is (= [{:x 1}
            {:x 2}
            {:x 3}]
           result))))

(deftest with-rebind
  (let [data []
        result (q "WITH (INSERT INTO data (x) VALUES (1)) AS data,
                              (INSERT INTO data (x) VALUES (2)) AS data,
                              (INSERT INTO data (x) VALUES (3)) AS data:
                         SELECT * FROM data;"
                  data)]
    (is (= [{:x 1}
            {:x 2}
            {:x 3}]
           result))))

;; Incorporate Column

;; FIXME
#_
(def incorporate-test-data
  {0 {:color "red" :flip true}
   1 {:color "red" :flip true}
   2 {:color "red" :flip true}
   3 {:color "red" :flip false}
   4 {:color "blue" :flip false}
   5 {:color "green" :flip false}})

;; FIXME
#_
(def incorporate-test-xcat-model
  (let [options {:color ["red" "blue" "green"]}
        view-1-name (gensym)
        view-2-name (gensym)
        xcat-spec {:views {view-1-name {:hypers {:color {:alpha 2}}}
                           view-2-name {:hypers {:flip {:alpha 1 :beta 1}}}}
                   :types {:color  :categorical
                           :flip :bernoulli}}

        xcat-latents {:global {:alpha 0.5}
                      :local {view-1-name {:alpha 1
                                           :counts {:one 4 :two 2}
                                           :y {0 :one
                                               1 :one
                                               2 :one
                                               3 :one
                                               4 :two
                                               5 :two}}
                              view-2-name {:alpha 1
                                           :counts {:one 3 :two 3}
                                           :y {0 :one
                                               1 :one
                                               2 :one
                                               3 :two
                                               4 :two
                                               5 :two}}}}]
    (xcat/construct-xcat-from-latents xcat-spec xcat-latents incorporate-test-data {:options options})))

;; FIXME
#_
(deftest incorporate-column
  (let [row-order (-> (keys incorporate-test-data)
                      (sort))
        data (mapv #(get incorporate-test-data %) row-order)
        model incorporate-test-xcat-model
        query "WITH (INCORPORATE COLUMN (1=true, 2=true) AS label INTO model) AS model:
               SELECT (PROBABILITY DENSITY OF label=true
                       UNDER model
                       CONDITIONED BY color AND flip
                       AS prob)
               FROM data;"
        result (q query data {:model model})
        uniq-probs (sort (distinct (map :prob result)))
        [low-p high-p] uniq-probs]
    ;; Our model has two views. The :label column gets incorporated into
    ;; the first view.
    ;; Within the first view, there are two clusters.
    ;; All postively labeled rows are in the first cluster, so all rows
    ;; in that cluster should have a higher probability.
    (is (= 2 (count uniq-probs)))
    (is (= (map :prob result) [high-p high-p high-p high-p low-p low-p]))))

;;; Condition by

(deftest conditioned-by
  (testing "generate"
    (let [q #(q % (with-meta [] {:iql/columns [:y]}) {:model simple-model})]
      (doseq [result (q "SELECT * FROM (GENERATE VAR x UNDER model CONDITIONED BY VAR x = \"yes\") LIMIT 10")]
        (is (= {:x "yes"} (select-keys result [:x]))))
      (doseq [result (q "WITH \"yes\" AS v: SELECT * FROM (GENERATE VAR x UNDER model CONDITIONED BY VAR x = v) LIMIT 10")]
        (is (= {:x "yes"} (select-keys result [:x]))))
      (doseq [result (q "WITH \"yes\" AS v: SELECT * FROM (GENERATE VAR x UNDER model CONDITIONED BY VAR x = v) LIMIT 10")]
        (is (= {:x "yes"} (select-keys result [:x]))))))

  (let [q (fn [query rows]
            (-> (q query rows {:model simple-model})
                (first)
                (vals)
                (first)))]
    (testing "logpdf"
      (testing "condition present"
        (is (= 0.0 (q "SELECT PROBABILITY DENSITY OF VAR x = \"yes\" UNDER model CONDITIONED BY VAR y = y FROM data"
                      [{:y "no"}]))))
      (testing "condition missing"
        (testing "in select"
          (is (= 0.75 (q "SELECT PROBABILITY DENSITY OF VAR x = \"yes\" UNDER model CONDITIONED BY VAR y = y FROM data"
                         (with-meta [{}]
                           {:iql/columns [:x :y]})))))
        (testing "in with"
          (is (= 0.0 (q "WITH model CONDITIONED BY VAR y = \"no\" AS model: SELECT PROBABILITY DENSITY OF VAR x = x UNDER model FROM data" [{:x "yes"}]))))))))
