(ns inferenceql.query.sequence-test
  (:require [clojure.test :refer [are deftest]]
            [inferenceql.query.sequence :as sequence]))

(deftest intersperse
  (are [expected c1 c2] (= expected (sequence/intersperse c1 c2))
    [] [] []
    [:a] [:a] []
    [] [] [:x]
    [:a] [:a] [:x]
    [:a] [:a] [:x :y]
    [:a :x :b] [:a :b] [:x]
    [:a :x :y :b] [:a :b] [:x :y]
    [:a :x :b :x :c] [:a :b :c] [:x]
    [:a :x :y :b :x :y :c] [:a :b :c] [:x :y]))
