(ns inferenceql.query.tuple-test
  (:refer-clojure :exclude [get])
  (:require [clojure.test :refer [deftest are]]
            [inferenceql.query.tuple :as tuple]))

(deftest ->map
  (are [expected m name] (= expected
                            (-> (tuple/tuple m (when name {:name name}))
                                (tuple/->map)))
    '{} '{} nil
    '{} '{} 'data
    '{x 0} '{x 0} nil
    '{x 0 data.x 0} '{x 0} 'data ) )

(deftest get
  (are [expected m name attr] (= expected
                                 (-> (tuple/tuple m (when name {:name name}))
                                     (tuple/get attr)))
    nil {} nil 'x
    nil {} 'data 'x
    0 '{x 0} nil 'x
    0 '{x 0} 'data 'x
    0 '{x 0 y 1} nil 'x
    0 '{x 0 y 1} 'data 'x))
