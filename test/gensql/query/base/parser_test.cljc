(ns gensql.query.base.parser-test
  "Tests for parsing common GenSQL queries. Tests against both strict and
  permissive parsers. Tests against either the strict or permissive variants
  should go in their respective test files."
  (:require [clojure.test :refer [deftest testing is]]
            [gensql.query.parser.tree :as tree]
            [gensql.query.strict.parser :as strict.parser]
            [gensql.query.permissive.parser :as permissive.parser]
            [instaparse.core :as insta]))

(defmacro parses
  [s k]
  `(do (is (not (insta/failure? (strict.parser/parse ~s :start ~k))))
       (is (not (insta/failure? (permissive.parser/parse ~s :start ~k))))))

(defmacro does-not-parse
  [s k]
  `(do (is (insta/failure? (strict.parser/parse ~s :start ~k)))
       (is (insta/failure? (permissive.parser/parse ~s :start ~k)))))

(defn parses-as
  ([s tag]
   (parses-as s tag :identifier))
  ([s tag k]
   (do
     (is (= tag (-> (strict.parser/parse s :start k) tree/only-child tree/tag)))
     (is (= tag (-> (permissive.parser/parse s :start k) tree/only-child tree/tag))))))

(deftest parse-simple-symbol-success
  (parses "a" :simple-symbol)
  (parses "A" :simple-symbol)
  (parses "a0" :simple-symbol)
  (parses "a0a" :simple-symbol)
  (parses "a-" :simple-symbol)
  (parses "a-a" :simple-symbol)
  (parses "a?" :simple-symbol)

  (testing "foreign unicode letters"
    (parses "デジタルガレージ" :simple-symbol)
    (parses "โหระพา_2" :simple-symbol)))

(deftest parse-simple-symbol-failure
  (does-not-parse "0a" :simple-symbol)
  (does-not-parse "#a" :simple-symbol)
  (does-not-parse "a#" :simple-symbol)
  (does-not-parse "$a" :simple-symbol)
  (does-not-parse "a$" :simple-symbol)
  (does-not-parse "%a" :simple-symbol)
  (does-not-parse "a%" :simple-symbol)
  (does-not-parse "^a" :simple-symbol)
  (does-not-parse "a^" :simple-symbol)
  (does-not-parse "@a" :simple-symbol)
  (does-not-parse "a@" :simple-symbol)
  (does-not-parse "&a" :simple-symbol)
  (does-not-parse "a&" :simple-symbol)
  (does-not-parse "|a" :simple-symbol)
  (does-not-parse "a|" :simple-symbol)
  (does-not-parse "[a" :simple-symbol)
  (does-not-parse "a[" :simple-symbol)
  (does-not-parse "]a" :simple-symbol)
  (does-not-parse "a]" :simple-symbol)
  (does-not-parse "(a" :simple-symbol)
  (does-not-parse "a(" :simple-symbol)
  (does-not-parse ")a" :simple-symbol)
  (does-not-parse "a)" :simple-symbol)
  (does-not-parse "{a" :simple-symbol)
  (does-not-parse "a{" :simple-symbol)
  (does-not-parse "}a" :simple-symbol)
  (does-not-parse "a}" :simple-symbol))

(deftest parse-identifier-category-correctly
  (testing "simple-symbol"
    (parses-as "a" :simple-symbol)
    (parses-as "A" :simple-symbol)
    (parses-as "a0" :simple-symbol)
    (parses-as "a0a" :simple-symbol)
    (parses-as "a-" :simple-symbol)
    (parses-as "a-a" :simple-symbol)
    (parses-as "a?" :simple-symbol))

  (testing "delimited-symbol"
    (parses-as "\" oddity\"" :delimited-symbol)
    (parses-as "\"mazzy*\"" :delimited-symbol)
    (parses-as "\"$parton\"" :delimited-symbol)
    (parses-as "\"|bie-girl\"" :delimited-symbol)
    (parses-as "\"()s just don't understand\"" :delimited-symbol)
    (parses-as "\"king of ^ flowers\"" :delimited-symbol)
    (parses-as "\"!@{}[]#$%^&*()_\"" :delimited-symbol)))

(deftest parse-string-success
  (parses "''" :string)
  (parses "'x'" :string)
  (parses "'xy'" :string)
  (parses "'0'" :string)
  (parses "'01'" :string)
  (parses "'x0'" :string)
  (parses "'0x'" :string))

(deftest parse-string-failure
  (does-not-parse "" :string)
  (does-not-parse "x" :string)
  (does-not-parse "\"\"" :string)
  (does-not-parse "\"x\"" :string)
  (does-not-parse "'" :string)
  (does-not-parse "'x" :string)
  (does-not-parse "x'" :string)
  (does-not-parse "'''" :string)
  (does-not-parse "'x''" :string)
  (does-not-parse "''x'" :string))

(deftest parse-generative-join-success
  (parses "table GENERATIVE JOIN model" :generative-join-expr))

(deftest parse-generate-star
  (parses "GENERATE * UNDER model" :generate-expr))

(deftest select-star
  (parses "SELECT *" :select-clause)
  (parses "SELECT * EXCEPT foo" :select-clause)
  (parses "SELECT * EXCEPT foo, bar" :select-clause)
  (parses "SELECT * EXCEPT (foo, bar)" :select-clause)
  (parses "SELECT * EXCEPT(x,y,z)" :select-clause))
