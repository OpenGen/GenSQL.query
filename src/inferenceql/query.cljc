(ns inferenceql.query
  "This file defines functions for parsing, transforming, and executing IQL-SQL
  queries."
  (:refer-clojure :exclude [eval])
  (:require [clojure.spec.alpha :as s]
            [hashp.core]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.plan :as plan]
            [inferenceql.query.plan.environment :as env]
            [inferenceql.query.relation :as relation]
            [instaparse.core :as insta]
            [medley.core :as medley]))

(def default-table
  "The name used for the table provided to `q`."
  ^:prviate
  'data)

(s/def ::plan (s/keys :req [::plan/plan ::env/plan]))

(defn plan
  [node]
  (case (tree/tag node)
    :query (recur (tree/only-child-node node))
    :relation-expr {::plan/plan (plan/plan node)}
    :with-expr {::plan/plan (plan/plan (tree/get-node node :relation-expr))
                ::env/plan (env/plan node)}))

(defn eval
  [plan env]
  (let [env (env/eval (::env/plan plan) env)]
    (plan/eval (::plan/plan plan) env)))

(defn q
  "Returns the result of executing a query on a set of rows. A registry
  mapping model names to model values models can be provided as an optional
  third argument."
  ([query rows]
   (q query rows {}))
  ([query rows models]
   (let [node-or-failure (parser/parse query)]
     (if-not (insta/failure? node-or-failure)
       (let [plan (plan node-or-failure)
             tuples (map #(medley/map-keys symbol %) rows)
             in-rel (if-let [columns (-> rows meta :iql/columns)]
                      (relation/relation tuples (map symbol columns))
                      (relation/relation tuples))
             models (medley/map-keys symbol models)
             env (merge models {(symbol default-table) in-rel})
             out-rel (eval plan env)
             kw-rel (map #(medley/map-keys keyword %)
                         out-rel)]
         (with-meta kw-rel
           {:iql/columns (map keyword (relation/attributes out-rel))}))
       (let [failure (insta/get-failure node-or-failure)
             ex-map {:cognitect.anomalies/category :cognitect.anomalies/incorrect
                     :instaparse/failure failure}]
         (throw (ex-info "Parsing failure" ex-map)))))))


(comment
 (require '[inferenceql.query.parser :as parser] :reload)

 (eval (plan
        (parser/parse "with select x, y from data as data2: select x + 1, y - 1 from data2;")
        )
       '{data [{x 0 y 1 z 2}
               {x 1 y 2 z 3}
               {x 2 y 3 z 4}]})

 (plan
  (parser/parse "with model conditioned by var x = 3 as model: select probability of var x = 3 under model from data;")
  )

 ,)
