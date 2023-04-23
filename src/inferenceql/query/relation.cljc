(ns inferenceql.query.relation
  "Functions for creating and manipulating relations."
  (:refer-clojure :exclude [distinct empty group-by name sort sequence])
  (:import [clojure.lang Associative IFn ILookup IPersistentCollection MapEntry Seqable Sequential]
           [java.lang Object])
  (:require [clojure.core :as clojure]
            [inferenceql.query.tuple :as tuple]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

;; These are not sequence functions, so the primary argument comes first.

(deftype Relation [name attributes ^clojure.lang.IPersistentCollection rows]
  Associative
  (assoc [_ k v]
    (assoc rows k v))
  (containsKey [_ k]
    (contains? rows k))
  (entryAt [_ k]
    (get rows k))

  IFn
  (invoke [this k]
    (.valAt this k))

  ILookup
  (valAt [_ k]
    (get rows k))
  (valAt [_ k not-found]
    (get rows k not-found))

  IPersistentCollection
  (cons [_ o]
    (assert (map? o))
    (assert (every? #(contains? (set attributes) %)
                    (keys o)))
    (Relation. name attributes (conj rows o)))
  (count [_]
    (count rows))
  (empty [_]
    (Relation. name attributes []))
  (equiv [this o]
    (if (instance? Relation o)
      (let [^Relation o o]
        (and (= (.-name this)
                (.-name o))
             (= (.-attributes this)
                (.-attributes o))
             (= (.-rows this)
                (.-rows o))))
      (= rows o)))

  Seqable
  (seq [_]
    (seq rows))

  ;; Iterable
  ;; (iterator [this]
  ;;   (prn "this" this)
  ;;   (.iterator
  ;;    ^java.lang.Iterable
  ;;    (.seq this)))

  Object
  (toString [this]
    (pr-str this))

  Sequential)

#?(:clj (defmethod print-method Relation [^Relation rel ^java.io.Writer w]
          (.write w (pr-str (.-rows rel)))))

(defn relation
  "Produces a relation from a sequence of maps."
  [coll & {:keys [attrs name]}]
  (let [attrs (or attrs
                  (into []
                        (comp (mapcat keys)
                              (clojure/distinct))
                        coll))]
    (Relation. name attrs coll)))

(defn empty
  "Returns an empty relation with the specified attributes."
  [attrs]
  (relation [] :attrs attrs))

(defn relation?
  "Returns true if `x` is a relation."
  [x]
  (= Relation (type x)))

(defn attributes
  "Returns the attributes of a relation."
  [^Relation rel]
  (.-attributes rel))

(defn name
  [^Relation rel]
  (.-name rel))

(defn assoc-name
  [^Relation rel name]
  (let [^Relation rel (or rel (relation []))]
    (Relation. name
               (.-attributes rel)
               (.-rows rel))))

clojure/sequence
(defn sequence
  [xf ^Relation rel]
  (Relation. (.-name rel)
             (.-attributes rel)
             (clojure/sequence xf (.-rows rel))))

(defn tuples
  "Returns a sequence of tuples in relation `rel`."
  [^Relation rel]
  (let [xf (map #(tuple/tuple %
                              :attrs (attributes rel)
                              :name (name rel)))]
    (clojure/sequence xf rel)))

(defn project
  [rel attrs]
  (sequence (map #(tuple/select-attrs % attrs))
            rel))

(defn extended-project
  "Takes a relation and a collection of function / attribute pairs. Returns a
  new relation with the attributes from the collection. Attribute values are
  produced by applying the functions from coll to each tuple in the relation
  argument."
  [rel coll]
  (let [f (fn [tuple]
            (->> (zipmap (map second coll)
                         (map #((first %) tuple)
                              coll))
                 (medley/remove-vals nil?)))]
    (relation (map f (tuples rel))
              :attrs (mapv second coll))))

(defn select
  [rel pred]
  (sequence (filter pred) rel))

(defn limit
  [rel n]
  (sequence (take n) rel))

(defn distinct
  [rel]
  (sequence (clojure/distinct) rel))

(defn sort
  [rel attr order]
  (let [rows (->> (tuples rel)
                  (sort-by #(tuple/get % attr)
                           (case order
                             :ascending compare
                             :descending #(compare %2 %1))))]
    (relation rows :attrs (attributes rel))))

(defn add-attribute
  [^Relation rel attr]
  (let [attributes (into []
                         (clojure/distinct)
                         (conj (attributes rel)
                               attr))]
    (Relation. (.-name rel)
               attributes
               (.-rows rel))))

(defn group-by
  [rel f]
  (->> (tuples rel)
       (clojure/group-by f)
       (vals)
       (map #(relation % :attrs (attributes rel)))))

(defn ->vector
  [rel]
  (let [attrs (attributes rel)]
    (into [attrs]
          (mapv (comp vec tuple/->vector)
                (tuples rel)))))

(comment

  (let [x [{:x 0} {:x 1} {:x 2}]]
    (= x (relation x)))

  (relation? (relation [{:x 0} {:x 1} {:x 2}]))
  (attributes (relation [{:x 0} {:x 1} {:x 2}]))
  (seq (relation [{:x 0} {:x 1} {:x 2}]))
  ((relation [{:x 0} {:x 1} {:x 2}]) 1)
  (nth (relation [{:x 0} {:x 1} {:x 2}]) 1)
  (count (relation [{:x 0} {:x 1} {:x 2}]))

  (sequence (relation [{:x 0} {:x 0} {:x 2}])
            (map #(assoc % :y 0)))

  (distinct (relation [{:x 0} {:x 0} {:x 2}]))

  (let [rel (relation [{:x 0 :y "hello"} {:x 0 :y "world"} {:x 2}])]
    (->> rel
         (tuples)
         (clojure/group-by :x)
         (vals)
         (map #(relation % :attrs (attributes rel)))))

  __rows

  (-> (relation [{:x 0} {:x 0} {:x 2}])
      (tuples)
      (type))

  (conj (relation [{:x 0} {:x 0} {:x 2}])
        {:y 0})

  (sort (relation [{:x 0} {:x 0} {:x 2}])
        :x
        :ascending)

  (set! *print-length* 10)
  ,)
