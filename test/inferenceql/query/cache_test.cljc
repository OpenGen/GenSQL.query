(ns inferenceql.query.cache-test
  (:require [clojure.test :refer [deftest is testing]]
            [inferenceql.query.cache :as cache]))

(deftest basic-caching
  (let [cache-size 2
        a (atom 0)
        incrementer (fn [_ignored-but-cached-key]
                      (swap! a inc))
        cached-incrementer (cache/lru incrementer cache-size)]

    (is (= 1 (cached-incrementer :foo)))
    (is (= 2 (cached-incrementer :bar)))

    (is (= 1 (cached-incrementer :foo)))
    (is (= 2 (cached-incrementer :bar)))

    (is (= 3 (cached-incrementer :moop)))

    ;; cache cleared for :foo
    (is (= 4 (cached-incrementer :foo)))))

(deftest disambiguate-between-0-and-nil
  (let [cache-size 1000
        englishize (fn [x]
                     (case x
                       0 "zero"
                       nil "nil"
                       "other"))
        cached-englishize (cache/lru englishize cache-size)]
    ;; Add them both.
    (is (= "zero" (cached-englishize 0)))
    (is (= "nil" (cached-englishize nil)))

    ;; Check that they return the correct values.
    (is (= "zero" (cached-englishize 0)))
    (is (= "nil" (cached-englishize nil)))))
