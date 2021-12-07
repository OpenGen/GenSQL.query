(ns inferenceql.query.js-test
  (:require [cljs-bean.core :as bean]
            [clojure.test :as test :refer [deftest is]]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.js :as query.js]))

(deftest select
  (is (= [{:x 1} {:x 2}]
         (bean/->clj (query.js/query "SELECT * FROM data WHERE x > 0;"
                                     #js[#js{:x 0}
                                         #js{:x 1}
                                         #js{:x 2}]
                                     #js{})))))

(deftest generate
  (let [model (gpm/Multimixture
               {:vars {:x :categorical}
                :views [[{:probability 1
                          :parameters {:x {"a" 1}}}]]})]
    (is (= [{:x "a"}]
           (bean/->clj (query.js/query "SELECT * FROM GENERATE * UNDER model LIMIT 1"
                                       #js[]
                                       #js{:model model}))))))
