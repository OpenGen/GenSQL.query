(ns inferenceql.query.string
  (:refer-clojure :exclude [munge demunge])
  (:require [clojure.set :as set]
            [clojure.string :as string]))

(defn upper-case?
  "Returns true if s is all upper-case. Otherwise returns false."
  [s]
  (= s (string/upper-case s)))

(defn lower-case?
  "Returns true if s is all lower-case. Otherwise returns false."
  [s]
  (= s (string/lower-case s)))

(defn match-case
  "If s2 is all upper-case or lower-case changes s1 to match."
  [s1 s2]
  (cond (upper-case? s2)
        (string/upper-case s1)

        (lower-case? s2)
        (string/lower-case s1)

        :else
        s1))
