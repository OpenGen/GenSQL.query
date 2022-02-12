(ns inferenceql.query.environment
  (:refer-clojure :exclude [get])
  (:require [clojure.spec.alpha :as s]))

(defn get
  "Look up a symbol in the given environment."
  [env bindings sym]
  (let [env (merge env bindings)]
    (clojure.core/get env sym)))

(defn env?
  "Returns `true` if `x` is an environment."
  [x]
  (map? x))

(defn ->map
  [env]
  env)

(s/def ::env env?)
(s/def ::sym symbol?)
