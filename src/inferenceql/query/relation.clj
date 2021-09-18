(ns inferenceql.query.relation
  (:refer-clojure :exclude [empty])
  (:require [inferenceql.query.tuple :as tuple]))

;; These are not sequence functions, so the primary argument comes first.

(defn relation
  [coll attributes]
  (with-meta coll {::attributes attributes}))

(defn empty
  "Returns an empty relation with the specified attributes."
  [attrs]
  (with-meta [] {::attributes attrs}))

(defn relation?
  "Returns true if `x` is a relation."
  [x]
  (and (coll? x)
       (contains? (meta x)
                  ::attributes)))

(defn attributes
  "Returns the attributes of a relation."
  [rel]
  (-> rel meta ::attributes))

(defn tuples
  "Returns a sequence of tuples in relation `rel`."
  [rel]
  (map #(with-meta % {::attributes (attributes rel)})
       rel))

(defn project
  [rel attrs]
  (into (empty attrs)
        (map #(tuple/select-attrs % attrs))
        (tuples rel)))

(defn extended-project
  [rel coll]
  (let [f (fn [tuple]
            (zipmap (map second coll)
                    (map #((first %) tuple)
                         coll)))]
    (into (empty (map second coll))
          (map f)
          (tuples rel))))

(defn select
  [rel pred]
  (into (empty (attributes rel))
        (filter #(tuple/apply-pred % pred))
        (tuples rel)))
