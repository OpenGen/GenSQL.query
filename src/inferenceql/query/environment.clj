(ns inferenceql.query.environment
  (:refer-clojure :exclude [get]))

(defn get
  "Look up a symbol in the given environment."
  [env sym]
  (clojure.core/get env sym))

(defn ->map
  [env]
  env)
