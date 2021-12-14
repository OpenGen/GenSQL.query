(ns inferenceql.query.relation-test
  (:refer-clojure :exclude [name])
  (:require [clojure.test :refer [deftest is]]
            [inferenceql.query.relation :as relation]))

(deftest name
  (is (= 'x (relation/name (relation/relation [] :name 'x))))
  (is (= nil (relation/name (relation/relation [])))))
