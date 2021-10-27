(ns inferenceql.query.literal
  (:refer-clojure :exclude [read])
  (:require [clojure.core.match :as match]
            [clojure.edn :as edn]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.relation :as relation]))

(declare read)

(defn ^:private read-relation
  [node]
  (let [[symbol-list-node value-list-nodes] (tree/child-nodes node)
        attributes (map read (tree/child-nodes symbol-list-node))
        values (map #(map read (tree/child-nodes %)) (tree/child-nodes value-list-nodes))
        m (map #(zipmap attributes %)
               values)]
    (relation/relation m attributes)))

(defn read
  [node]
  (match/match [node]
    [[:value child]] (recur child)

    [[:bool s]]          (edn/read-string s)
    [[:float s]]         (edn/read-string s)
    [[:int s]]           (edn/read-string s)
    [[:nat s]]           (edn/read-string s)
    [[:simple-symbol s]] (edn/read-string s)
    [[:string s]]        (edn/read-string s)

    [[:null _]] nil
    [nil] nil

    [[:relation-expr child]] (recur child)

    [(node :guard (tree/tag-pred :relation-value))] (read-relation node)))
