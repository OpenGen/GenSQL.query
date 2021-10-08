(ns inferenceql.query.plan.environment
  (:refer-clojure :exclude [eval])
  (:require [clojure.core.match :as match]
            [clojure.spec.alpha :as s]
            [inferenceql.query.environment :as env]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.plan :as plan]
            [inferenceql.query.scalar :as scalar]
            [medley.core :as medley]))

(s/def ::symbol symbol?)

(s/def ::init-plan
  (s/or :relation ::plan/plan
        :scalar ::scalar/plan))

(s/def ::plan (s/map-of ::env/name ::init-plan))

(defn expr-plan
  [node]
  (case (tree/tag node)
    :relation-expr (plan/plan node)
    :scalar-expr (scalar/plan node)
    :model-expr (scalar/plan node)
    (recur (first (tree/child-nodes node)))))


(defn plan
  [node]
  (case (tree/tag node)
    :relation-expr
    {}

    :with-expr
    (let [children (tree/child-nodes node)
          binding-nodes (butlast children)]
      (into {}
            (map (juxt tree/alias expr-plan))
            binding-nodes))))

(defn eval-init
  [plan env]
  (match/match (s/conform ::init-plan plan)
    [:scalar plan] (scalar/eval plan env {})
    [:relation plan] (plan/eval plan env)))

(defn eval
  [node env]
  (merge env (medley/map-vals #(eval-init % env) node)))

(comment

 (set! *print-length* 10)

 (require '[inferenceql.query.parser :as parser] :reload)

 (plan/plan (parser/parse "select * from data"))

 ,)
