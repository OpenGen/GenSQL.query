(ns inferenceql.query.string-test
  (:require [clojure.test :refer [are deftest]]
            [inferenceql.query.string :as string]))

(deftest upper-case?
  (are [b s] (= b (string/upper-case? s))
    true "A"
    true "AB"
    true "ABC"
    false "a"
    false "ab"
    false "abc"
    false "aB"
    false "Ab"
    false "aBC"
    false "AbC"
    false "ABc"))

(deftest lower-case?
  (are [b s] (= b (string/lower-case? s))
    true "a"
    true "ab"
    true "abc"
    false "A"
    false "AB"
    false "ABC"
    false "Ab"
    false "aB"
    false "Abc"
    false "aBc"
    false "abC"))

(deftest match-case
  (are [expected s1 s2] (= expected (string/match-case s1 s2))
    "ABC" "ABC" "X"
    "abc" "abc" "x"
    "abc" "ABC" "x"
    "ABC" "abc" "X"
    "abc" "abc" "Xy"
    "abc" "abc" "xY"
    "ABC" "ABC" "Xy"
    "ABC" "ABC" "xY"
    "aBc" "aBc" "Xy"
    "AbC" "AbC" "xY"))

(deftest munge-roundtrip
  (are [orig] (= orig (string/demunge (string/munge orig)))
    "abc"
    "ABC"
    "has-dash"
    "has space"
    "I'd buy that for a dollar!"
    "[weird sql quoting syntax]"
    "`~!@#$%^&*(){}[];:\\\",.?_-+="
    :foo-bar
    :foo/bar
    'moop
    'moop/bloop))

(deftest munge-idempotency
  ;; Ensures that calling munge twice on the same string is the same as calling it once
  (are [orig] (= (string/munge orig) (string/munge (string/munge orig)))
    "abc"
    "ABC"
    "has-dash"
    "has space"
    "I'd buy that for a dollar!"
    "[weird sql quoting syntax]"
    "`~!@#$%^&*(){}[];:\\\",.?_-+="
    :foo-bar
    :foo/bar
    'moop
    'moop/bloop))

