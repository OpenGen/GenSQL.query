(ns inferenceql.query
  "This file defines functions for parsing, transforming, and executing IQL-SQL
  queries. The public API for this file is the functions are `q`, `pq`, and
  `query-plan`."
  (:require [datascript.core :as d]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.lang.eval :as eval]
            [inferenceql.query.lang.condition]
            [inferenceql.query.lang.constrain]
            [inferenceql.query.lang.literals]
            [inferenceql.query.lang.select :as select]
            [inferenceql.query.lang.model]
            [inferenceql.query.lang.table]
            [inferenceql.query.lang.with-as]
            [inferenceql.query.math :as math]
            [instaparse.core :as insta]))

(def default-table :data)

(def default-environment
  {`math/exp math/exp
   `merge merge
   `d/entity d/entity
   `d/pull d/pull
   `gpm/logpdf gpm/logpdf
   `gpm/logprob gpm/logprob

   `=  =
   `not= not=
   `>  >
   `>= >=
   `<  <
   `<= <=})

(defn q
  "Returns the result of executing a query on a set of rows. A registry
  mapping model names to model values models can be provided as an optional
  third argument."
  ([query rows]
   (q query rows {}))
  ([query rows models]
   (let [node-or-failure (parser/parse query)]
     (if-not (insta/failure? node-or-failure)
       (let [rows (select/add-placeholders rows)
             env (merge default-environment models {default-table rows})]
         (eval/eval node-or-failure env))
       (let [failure (insta/get-failure node-or-failure)
             ex-map {:cognitect.anomalies/category :cognitect.anomalies/incorrect
                     :instaparse/failure failure}]
         (throw (ex-info "Parsing failure" ex-map)))))))

(comment

 (require '[hashp.core])
 (require '[inferenceql.query.relation :as relation])
 (parser/parse "select * from data")

 )
