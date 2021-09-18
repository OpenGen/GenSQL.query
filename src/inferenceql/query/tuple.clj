(ns inferenceql.query.tuple
  (:require [medley.core :as medley]))

(defn tuple?
  [x]
  (map? x))

(defn attr=
  [attr1 attr2]
  (= (name attr1)
     (name attr2)))

(defn select-attrs
  [tup attrs]
  (let [names (into #{} (map name) attrs)]
    (medley/filter-keys (comp names name)
                        tup)))

(defn apply-pred
  [tup pred]
  (pred (medley/map-keys symbol tup)))

(defn ->map
  "Converts tuple `tup` to an immutable hash map."
  [tup]
  tup)

(comment

 (select-attrs {:x 0 :y 1} [:x])
 (select-attrs {:x 0 :y 1} ['x])
 (select-attrs {:x 0 :y 1} ["x"])

 ,)
