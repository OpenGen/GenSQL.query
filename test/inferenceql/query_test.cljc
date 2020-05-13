(ns inferenceql.query-test
  (:require [clojure.string :as string]
            [clojure.test :as test :refer [are deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.generators :as chuck.gen]
            [instaparse.core :as insta]
            [inferenceql.query :as query]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.inference.multimixture.specification :as mmix.spec]))

(def simple-mmix
  {:vars {:x :categorical
          :y :categorical}
   :views [[{:probability 0.75
             :parameters  {:x {"yes" 1.0 "no" 0.0}
                           :y {"yes" 1.0 "no" 0.0}}}
            {:probability 0.25
             :parameters  {:x {"yes" 0.0 "no" 1.0}
                           :y {"yes" 0.0 "no" 1.0}}}]]})

;;; inferenceql.query/constrain

(defspec constrain
  (let [model (gpm/Multimixture simple-mmix)]
    (prop/for-all [x-value (gen/elements (mmix.spec/categories simple-mmix :x))
                   y-value (gen/elements (mmix.spec/categories simple-mmix :y))]
      (let [target {:x x-value}
            constraint {:y y-value}
            constrained-model (query/constrain model (mmix.spec/variables simple-mmix) constraint)]
        (is (= (gpm/logpdf model             target constraint)
               (gpm/logpdf constrained-model target {})))))))

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
  (gen/fmap symbol (gen/such-that (comp pos? count) string-alpha)))

(def gen-column
  (gen/fmap keyword gen-symbol))

(def gen-table
  "Generator for full \"tables\" (vectors of maps). Each row will have keys drawn
  from a consistent subset, and the values for each key will be drawn from a
  fixed generator."
  (let [value-generators [gen/small-integer gen/nat gen/int]]
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

(defn parse-and-transform-literals
  [& args]
  (->> (apply query/parse args)
       (insta/transform query/literal-transformations)))

(defspec nat-parsing
  (prop/for-all [n gen/nat]
    (let [s (pr-str n)]
      (is (= n (parse-and-transform-literals s :start :nat))))))

(defspec int-parsing
  (prop/for-all [n gen/int]
    (let [s (pr-str n)]
      (is (= n (parse-and-transform-literals s :start :int))))))

(defspec symbol-parsing
  (prop/for-all [sym gen-symbol]
    (let [s (pr-str sym)]
      (is (= sym (parse-and-transform-literals s :start :symbol))))))

;;; Column parsing

(deftest column-name-parsing
  (testing "valid"
    (are [s] (not (insta/failure? (query/parse s :start :column-name)))
      "a"
      "A"
      "a0"
      "a0a"
      "a-"
      "a-a"))
  (testing "invalid"
    (are [s] (insta/failure? (query/parse s :start :column-name))
      "0a"
      "-a")))

;; Float parsing is a bit different in CLJS. Among other things, whole-numbered
;; floats are printed without a decimal point. Someone should come back and try
;; to make this work in CLJS at some point.
#?(:clj (defspec float-parsing
          (prop/for-all [n (gen/double* {:infinite? false :NaN? false})]
            (let [s (pr-str n)]
              (is (== n (parse-and-transform-literals s :start :float)))))))

;;; Query parsing success/failure

(deftest parsing-success
  (are [start query] (nil? (insta/get-failure (query/parse query :start start)))
    :query "SELECT * FROM data"))

(deftest parsing-failure
  (are [start query] (some? (insta/get-failure (query/parse query :start start)))
    :query "123abc"))

;;; Basic selection

(deftest select-basic
  (let [rows [{:x 0}
              {:x 1}
              {:x 2}]]
    (testing "with source"
      (is (= rows (query/q "SELECT x FROM data" rows))))
    (testing "without source"
      (is (= rows (query/q "SELECT x" rows))))))

(defspec select-star
  (prop/for-all [table gen-table]
    (let [results (query/q "SELECT * FROM data" table)]
      (is (= results table)))))

(defspec select-col
  (prop/for-all [[table ks] gen-table-col-subset]
    (let [cols (->> ks (map name) (string/join ", "))
          results (query/q (str "SELECT " cols " FROM data") table)]
      (is (= results (map #(select-keys % ks)
                          table))))))

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
      (is (->> (query/q query table)
               (map col)
               (ascending?))))))

(defspec select-order-by-asc
  (prop/for-all [[table col] gen-table-col]
    (let [query (str "SELECT * FROM data ORDER BY " (name col) " ASC")]
      (is (->> (query/q query table)
               (map col)
               (ascending?))))))

(defspec select-order-by-desc
  (prop/for-all [[table col] gen-table-col]
    (let [query (str "SELECT * FROM data ORDER BY " (name col) " DESC") ]
      (is (->> (query/q query table)
               (map col)
               (descending?))))))

;;; Limit

(def gen-table-limit
  (gen/bind gen-table
            #(gen/tuple (gen/return %)
                        (gen/choose 0 (count %)))))

(defspec select-limit
  (prop/for-all [[table n] gen-table-limit]
    (let [results (query/q (str "SELECT * FROM data LIMIT " n) table)]
      (is (= (take n table)
             results)))))

;; Conditions

(defspec conditions-not-null
  (prop/for-all [[table k] gen-table-col]
    (let [results (query/q (str "SELECT * FROM data WHERE " (name k) " IS NOT NULL") table)]
      (is (= (remove (comp nil? k) table)
             results)))))

(defspec conditions-null
  (prop/for-all [[table k] gen-table-col]
    (let [results (query/q (str "SELECT * FROM data WHERE " (name k) " IS NULL") table)]
      (is (= (filter (comp nil? k) table)
             results)))))

(deftest conditions-precdence
  (testing "OR has higher precedence than AND"
    (let [query "SELECT * FROM data WHERE x=0 AND x=1 OR x=0"]
      (is (= [{:x 0}] (query/q query [{:x 0} {:x 1}]))))))

;; Probabilities

(deftest probability-of-bindings
  (let [rows [{}]
        models {:model (gpm/Multimixture simple-mmix)}
        q1 (comp first vals first #(query/q % rows models))]
    (is (= (Math/log 0.25) (q1 "SELECT (PROBABILITY OF x=\"no\"                  UNDER model) FROM data LIMIT 1")))
    (is (= (Math/log 0.75) (q1 "SELECT (PROBABILITY OF x=\"yes\"                 UNDER model) FROM data LIMIT 1")))
    (is (= (Math/log 1.0)  (q1 "SELECT (PROBABILITY OF x=\"yes\" GIVEN y=\"yes\" UNDER model) FROM data LIMIT 1")))
    (is (= (Math/log 1.0)  (q1 "SELECT (PROBABILITY OF x=\"no\"  GIVEN y=\"no\"  UNDER model) FROM data LIMIT 1")))
    (is (= (Math/log 0.0)  (q1 "SELECT (PROBABILITY OF x=\"yes\" GIVEN y=\"no\"  UNDER model) FROM data LIMIT 1")))))

(deftest probability-of-rows
  (let [models {:model (gpm/Multimixture simple-mmix)}
        q1 (comp first vals first #(query/q %1 %2 models))]
    (are [expected x] (is (= (Math/log expected)
                             (q1 "SELECT (PROBABILITY OF x UNDER model) FROM data"
                                 [{:x x}])))
      0.25 "no"
      0.75 "yes")

    (are [expected x y] (= (Math/log expected)
                           (q1 "SELECT (PROBABILITY OF x GIVEN y UNDER model) FROM data"
                               [{:x x :y y}]))
      1.0 "yes" "yes"
      1.0 "no"  "no"
      0.0 "yes" "no"
      0.0 "no"  "yes")))

(deftest probability-of-generate
  (is (= 1.0 (Math/exp (->> (query/q "SELECT (PROBABILITY OF x=\"yes\" UNDER (GENERATE x GIVEN y=\"yes\" UNDER model)) FROM data"
                                     [{}]
                                     {:model (gpm/Multimixture simple-mmix)})
                            first
                            vals
                            first)))))

;;; Generate

(deftest generate-generates-correct-columns
  (testing "Generate"
    (let [model (gpm/Multimixture simple-mmix)
          q #(query/q % [] {:model model})]
      (testing "with a single variable"
        (doseq [result (q "SELECT * FROM (GENERATE y UNDER model) LIMIT 10")]
          (is (= #{:y} (set (keys result))))))
      (testing "with multiple variables"
        (doseq [result (q "SELECT * FROM (GENERATE x, y UNDER model) LIMIT 10")]
          (is (= #{:x :y} (set (keys result))))))
      (testing "with a literal event"
        (doseq [result (q "SELECT * FROM (GENERATE x GIVEN x=\"yes\" UNDER model) LIMIT 10")]
          (is (= {:x "yes"} (select-keys result [:x])))))
      (testing "expressions can have a subset of columns selected from them"
        (doseq [result (q "SELECT y FROM (GENERATE x, y UNDER model) LIMIT 10")]
          (is (= [:y] (keys result)))))
      (testing "can be nested"
        (doseq [result (q "SELECT x, y FROM (GENERATE x, y UNDER (GENERATE x, y UNDER model)) LIMIT 10")]
          (is (= #{:x :y} (set (keys result)))))))))
