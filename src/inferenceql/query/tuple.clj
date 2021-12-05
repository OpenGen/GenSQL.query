(ns inferenceql.query.tuple
  "Functions for creating and manipulating tuples."
  (:refer-clojure :exclude [get])
  (:require [medley.core :as medley]))

(defn tuple
  [m attrs]
  (with-meta m {::attributes attrs}))

(defn tuple?
  "Returns `true` if `x` is a tuple, otherwise returns `false`."
  [x]
  (map? x))

(defn get
  "Retrieves the value for attribute `attr` from tuple `tup`."
  [tup attr]
  (clojure.core/get tup attr))

(defn select-attrs
  "Retrives tuple `tup` with only the attributes in `attrs`."
  [tup attrs]
  (let [names (into #{} (map name) attrs)]
    (medley/filter-keys (comp names name)
                        tup)))

(defn ->map
  "Converts tuple `tup` to an immutable hash map."
  [tup]
  tup)

(defn attributes
  "Returns the attributes of a tuple."
  [tup]
  (-> tup meta ::attributes))

(defn ->vector
  "Converts tuple `tup` to a vector.`"
  [tup]
  (map #(get tup %) (attributes tup)))
