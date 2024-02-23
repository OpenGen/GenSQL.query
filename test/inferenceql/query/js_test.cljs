(ns inferenceql.query.js-test
  (:require [cljs-bean.core :as bean]
            [clojure.test :as test :refer [deftest is]]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.db :as db]
            [inferenceql.query.js :as query.js]))

(defn db
  [tables models]
  (binding [*print-meta* true]
    (as-> (db/empty) %
      (reduce-kv db/with-table % tables)
      (reduce-kv db/with-model % models)
      (pr-str %))))

(deftest select
  (let [db (db {"data" [{"x" 0}
                        {"x" 1}
                        {"x" 2}]}
               {})]
    (is (= [{"x" 1} {"x" 2}]
           (mapv #(bean/bean % :keywordize-keys false)
                 (query.js/query "SELECT * FROM data WHERE x > 0;" db))))))

(deftest generate
  (let [model (gpm/Multimixture
               {:vars {"x" :categorical}
                :views [[{:probability 1
                          :parameters {"x" {"a" 1}}}]]})
        db (db {} {"model" model})]
    (is (= [{"x" "a"}]
           (mapv #(bean/bean % :keywordize-keys false)
                 (query.js/query "SELECT * FROM GENERATE * UNDER model LIMIT 1" db))))))
