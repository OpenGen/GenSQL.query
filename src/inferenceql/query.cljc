(ns inferenceql.query
  "This file defines functions for parsing, transforming, and executing IQL-SQL
  queries. The public API for this file is the functions are `q`, `pq`, and
  `query-plan`."
  (:require [inferenceql.query.lang.condition]
            [inferenceql.query.lang.constrain]
            [inferenceql.query.lang.literals]
            [inferenceql.query.lang.model]
            [inferenceql.query.lang.with-as]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.plan :as plan]
            [inferenceql.query.relation :as relation]
            [instaparse.core :as insta]
            [medley.core :as medley]))

(def default-table 'data)

(defn q
  "Returns the result of executing a query on a set of rows. A registry
  mapping model names to model values models can be provided as an optional
  third argument."
  ([query rows]
   (q query rows {}))
  ([query rows models]
   (let [node-or-failure (parser/parse query)]
     (if-not (insta/failure? node-or-failure)
       (let [plan (plan/plan node-or-failure)
             tuples (map #(medley/map-keys symbol %) rows)
             in-rel (if-let [columns (-> rows meta :iql/columns)]
                      (relation/relation tuples (map symbol columns))
                      (relation/relation tuples))
             models (medley/map-keys symbol models)
             env (merge models {(symbol default-table) in-rel})
             out-rel (plan/eval plan env)
             kw-rel (map #(medley/map-keys keyword %)
                         out-rel)]
         (with-meta kw-rel
           {:iql/columns (map keyword (relation/attributes out-rel))}))
       (let [failure (insta/get-failure node-or-failure)
             ex-map {:cognitect.anomalies/category :cognitect.anomalies/incorrect
                     :instaparse/failure failure}]
         (throw (ex-info "Parsing failure" ex-map)))))))

(comment

 (relation/attributes rel)
 (meta rel)

 (q "select x from data limit 2"
    [{:x 0}
     {:x 1}
     {:x 2}])

 ,)
