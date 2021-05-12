(ns inferenceql.query.lang.select.selections-test
  (:require [clojure.test :refer [deftest testing is]]
            [inferenceql.query :as query]))

(deftest value-selection
  (let [value 3
        limit 10]
    (testing "with AS"
      (let [query (str "SELECT " (pr-str value) " AS x LIMIT " limit)
            rows (for [x (range limit)]
                   {:x x})]
        (is (= (repeat limit {:x value})
               (query/q query rows)))))
    (testing "without AS"
      (let [query (str "SELECT " (pr-str value) " LIMIT " limit)
            rows (for [x (range limit)]
                   {:x x})]
        (is (= (repeat limit [value])
               (->> (query/q query rows)
                    (map vals))))))))
