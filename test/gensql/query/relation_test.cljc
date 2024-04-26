(ns gensql.query.relation-test
  (:refer-clojure :exclude [name])
  (:require [clojure.test :refer [deftest is]]
            [gensql.query.relation :as relation]))

(deftest name
  (is (= "x" (relation/name (relation/relation [] :name "x"))))
  (is (= nil (relation/name (relation/relation [])))))
