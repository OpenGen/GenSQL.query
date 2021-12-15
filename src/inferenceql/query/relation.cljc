(ns inferenceql.query.relation
  "Functions for creating and manipulating relations."
  (:refer-clojure :exclude [distinct empty group-by name sort transduce])
  (:require [clojure.core :as clojure]
            [clojure.spec.alpha :as s]
            [inferenceql.query.tuple :as tuple]
            [medley.core :as medley]))

;; These are not sequence functions, so the primary argument comes first.

(defn relation
  "Produces a relation from a sequence of maps."
  [coll & {:keys [attrs name]}]
  (let [attrs (or attrs
                  (into []
                        (comp (mapcat keys)
                              (clojure/distinct))
                        coll))]
    (cond-> coll
      (seq attrs) (vary-meta assoc ::attributes attrs)
      name (vary-meta assoc ::name name))))

(defn empty
  "Returns an empty relation with the specified attributes."
  [attrs]
  (relation [] :attrs attrs))

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

(defn name
  [rel]
  (-> rel meta ::name))

(defn assoc-name
  [rel name]
  (vary-meta (or rel []) assoc ::name name))

(defn tuples
  "Returns a sequence of tuples in relation `rel`."
  [rel]
  (map #(tuple/tuple %
                     :attrs (attributes rel)
                     :name (name rel))
       rel))

(defn project
  [rel attrs]
  (with-meta (sequence (map #(tuple/select-attrs % attrs))
                       (tuples rel))
    {::attributes attributes}))

(defn extended-project
  "Takes a relation and a collection of function / attribute pairs. Returns a
  new relation with the attributes from the collection. Attribute values are
  produced by applying the functions from coll to each tuple in the relation
  argument."
  [rel coll]
  (let [f (fn [tuple]
            tuple
            (meta tuple)
            (->> (zipmap (map second coll)
                         (map #((first %) tuple)
                              coll))
                 (medley/remove-vals nil?)))]
    (relation (map f (tuples rel))
              :attrs (mapv second coll))))

(defn select
  [rel pred]
  (with-meta (filter pred (tuples rel))
    (meta rel)))

(defn limit
  [rel n]
  (with-meta (take n (tuples rel))
    (meta rel)))

(defn distinct
  [rel]
  (with-meta (clojure/distinct (tuples rel))
    (meta rel)))

(defn sort
  [rel attr order]
  (relation (->> (tuples rel)
                 (sort-by #(tuple/get % attr)
                          (case order
                            :ascending compare
                            :descending #(compare %2 %1))))
            (attributes rel)))

(defn transduce
  [rel xf]
  (with-meta (into (empty rel)
                   xf
                   rel)
    (meta rel)))

(defn add-attribute
  [rel attr]
  (let [attributes (into []
                         (clojure/distinct)
                         (conj (attributes rel)
                               attr))]
    (relation rel :attrs attributes)))

(defn group-by
  [rel f]
  (->> (tuples rel)
       (clojure.core/group-by f)
       (vals)
       (map #(relation % (attributes rel)))))

(s/def ::name symbol?)
(s/def ::attribute symbol?)
(s/def ::relation relation?)
