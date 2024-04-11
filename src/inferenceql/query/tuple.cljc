(ns inferenceql.query.tuple
  "Functions for creating and manipulating tuples."
  (:refer-clojure :exclude [get name])
  (:require [clojure.core :as clojure]
            [clojure.string :as string]
            [medley.core :as medley]))

(defn tuple
  "Prepares a map to act as a tuple."
  [m & {:keys [attrs name]}]
  (with-meta m {::attributes attrs
                ::name name}))

(defn name
  "Returns the name of a tuple."
  [tup]
  (-> tup meta ::name))

(defn tuple?
  "Returns `true` if `x` is a tuple, otherwise returns `false`."
  [x]
  (map? x))

(defn select-attrs
  "Retrieves tuple `tup` with only the attributes in `attrs`."
  [tup attrs]
  (let [names (into #{} (map clojure.core/name) attrs)]
    (medley/filter-keys (comp names clojure.core/name)
                        tup)))

(defn ->map
  "Converts tuple `tup` to an immutable hash map."
  [tup]
  (merge tup
         (when-let [name (name tup)]
           ;; Sets the namespace of all keys to the tuple name
           (update-keys tup
                        #(string/join "."
                                      [(clojure/name name)
                                       (clojure/name %)])))))

(defn attributes
  "Returns the attributes of a tuple."
  [tup]
  (-> tup meta ::attributes))

(defn get
  "Retrieves the value for attribute `attr` from tuple `tup`."
  [tup attr]
  (clojure/get (->map tup) attr))

(defn ->vector
  "Converts tuple `tup` to a vector.`"
  [tup]
  (map #(get tup %) (attributes tup)))
