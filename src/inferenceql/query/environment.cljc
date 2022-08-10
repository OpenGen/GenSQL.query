(ns inferenceql.query.environment
  (:refer-clojure :exclude [get])
  (:require [clojure.core :as clojure]
            [cognitect.anomalies :as-alias anomalies]))

(defn get
  "Look up a symbol in an environment."
  [env bindings sym]
  (let [env (merge env bindings)]
    (clojure/get env sym)))

(defn has?
  "Returns true if a symbol exists in an environment."
  [env bindings sym]
  (let [env (merge env bindings)]
    (contains? env sym)))

(defn safe-get
  "Look up a symbol in the given environment."
  [env bindings sym]
  (if (has? env bindings sym)
    (get env bindings sym)
    (throw (ex-info (str "Could not resolve symbol: " (pr-str sym))
                    {::anomalies/category ::anomalies/incorrect
                     :symbol sym
                     :env env
                     :bindings bindings}))))

(defn env?
  "Returns `true` if `x` is an environment."
  [x]
  (map? x))

(defn ->map
  [env]
  env)
