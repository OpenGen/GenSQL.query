(ns inferenceql.query.literal-test
  (:refer-clojure :exclude [read])
  (:require [clojure.string :as string]
            [clojure.test :refer [are deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [inferenceql.query.literal :as literal]
            [inferenceql.query.strict.parser :as parser]
            [inferenceql.query.relation :as relation]))

(def gen-identifier
  (->> (gen/tuple gen/char-alpha
                  (gen/fmap string/join
                            (gen/vector (gen/frequency [[20 gen/char-alphanumeric]
                                                        [1 (gen/return \#)]
                                                        [1 (gen/return \$)]
                                                        [1 (gen/return \%)]
                                                        [1 (gen/return \&)]
                                                        [1 (gen/return \ )]
                                                        [1 (gen/return \()]
                                                        [1 (gen/return \))]
                                                        [1 (gen/return \-)]]))))
       (gen/fmap #(apply str %))
       (gen/such-that #(not (string/starts-with? % "G__")))
       #_(gen/fmap symbol)))

(deftest read-relation-metadata
  (let [rel (literal/read (parser/parse "(x, y) VALUES (0, 1)" :start :relation-value))]
    (is (relation/relation? rel))
    (is (= ["x" "y"] (relation/attributes rel)))))

(deftest read-relation-value
  (are [x s] (= x (literal/read (parser/parse s :start :relation-value)))
    [{"x" 0}]                             "(x) VALUES (0)"
    [{"x" 0} {"x" 1}]                     "(x) VALUES (0), (1)"
    [{"x" 0, "y" 1}]                      "(x, y) VALUES (0, 1)"
    [{"x" 0, "y" 1} {"x" 2, "y" 3}]       "(x, y) VALUES (0, 1), (2, 3)"
    [{"x" 0}]                             "(x) VALUES ... 0: (0) ..."
    [{"x" 0, "y" 1}]                      "(x, y) VALUES ... 0: (0, 1) ..."
    [{} {"x" 1}]                          "(x) VALUES ... 1: (1) ..."
    [{} {"x" 1, "y" 2}]                   "(x, y) VALUES ... 1: (1, 2) ..."
    [{} {"x" 1} {} {"x" 3}]               "(x) VALUES ... 1: (1), 3: (3) ..."
    [{} {"x" 1, "y" 2} {} {"x" 3, "y" 4}] "(x, y) VALUES ... 1: (1, 2), 3: (3, 4) ..."))

(defn parse-and-eval
  [& args]
  (-> (apply parser/parse args)
      (literal/read)))

(defspec nat-evaluation
  (prop/for-all [n gen/nat]
    (let [s (pr-str n)]
      (is (= n (parse-and-eval s :start :nat))))))

(defspec int-evaluation
  (prop/for-all [n gen/small-integer]
    (let [s (pr-str n)]
      (is (= n (parse-and-eval s :start :int))))))

(defspec id-evaluation
  (prop/for-all [s gen-identifier]
    (is (= s (parse-and-eval (str "\"" s "\"") :start :identifier)))))

;; Float parsing is a bit different in CLJS. Among other things, whole-numbered
;; floats are printed without a decimal point. Someone should come back and try
;; to make this work in CLJS at some point.
#?(:clj (defspec float-parsing
          (prop/for-all [n (gen/double* {:infinite? false :NaN? false})]
            (let [s (pr-str n)]
              (is (== n (parse-and-eval s :start :float)))))))
