(ns inferenceql.query.base.parser-test
  (:require [clojure.test :refer [deftest testing is]]
            [inferenceql.query.strict.parser :as strict.parser]
            [inferenceql.query.permissive.parser :as permissive.parser]
            [instaparse.core :as insta]))

(defmacro parses
  [s k]
  `(do (is (not (insta/failure? (strict.parser/parse ~s :start ~k))))
       (is (not (insta/failure? (permissive.parser/parse ~s :start ~k))))))

(defmacro does-not-parse
  [s k]
  `(do (is (insta/failure? (strict.parser/parse ~s :start ~k)))
       (is (insta/failure? (permissive.parser/parse ~s :start ~k)))))

(deftest parse-simple-symbol-success
  (parses "a" :simple-symbol)
  (parses "A" :simple-symbol)
  (parses "a0" :simple-symbol)
  (parses "a0a" :simple-symbol)
  (parses "a-" :simple-symbol)
  (parses "a-a" :simple-symbol)
  (parses "a?" :simple-symbol))

(deftest parse-simple-symbol-failure
  (does-not-parse "0a" :simple-symbol))
