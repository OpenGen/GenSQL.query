(ns gensql.query.environment
  (:refer-clojure :exclude [get])
  (:require [clojure.core :as clojure]
            [cognitect.anomalies :as-alias anomalies]))

(defn get
  "Look up an identifier in an environment. Returns ::not-found if missing."
  [env bindings id]
  (let [env (merge env bindings)]
    (clojure/get env id ::not-found)))

(defn has?
  "Returns true if an identifier exists in an environment."
  [env bindings id]
  (let [env (merge env bindings)]
    (contains? env id)))

(defn safe-get
  "Look up an identifier in the given environment. Throws an exception if not
  found."
  [env bindings id]
  (let [result (get env bindings id)]
    (if (= result ::not-found)
      (throw (ex-info (str "Could not resolve identifier: " (pr-str id))
                      {::anomalies/category ::anomalies/incorrect
                       :identifier id
                       :env env
                       :bindings bindings}))
      result)))

(defn env?
  "Returns `true` if `x` is an environment."
  [x]
  (map? x))

(defn ->map
  [env]
  env)
