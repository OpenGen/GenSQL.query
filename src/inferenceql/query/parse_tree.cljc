(ns inferenceql.query.parse-tree
  (:refer-clojure :exclude [find])
  (:require [clojure.string :as string]))

(defn branch?
  [node]
  (vector? node))

(defn node
  [tag children]
  (into [tag] children))

(defn tag
  [node]
  (when (branch? node)
    (first node)))

(defn whitespace?
  [node]
  (or (and (branch? node)
           (= :ws (tag node)))
      (and (string? node)
           (string/blank? node))))

(defn children
  [node]
  (into []
        (remove whitespace?)
        (rest node)))

(defn only-child
  [node]
  (let [children (children node)]
    (if-not (= 1 (count children))
      (throw (ex-info (str "Expected one child, but found " (count children))
                      {:node node}))
      (first children))))

(defn find-all
  [node pred]
  (filter pred (tree-seq branch? children node)))

(defn find
  [node pred]
  (first (find-all node pred)))

(defn find-all-tag
  [node tag]
  (find-all node #(= tag (inferenceql.query.parse-tree/tag %))))

(defn find-tag
  [node tag]
  (find node #(= tag (inferenceql.query.parse-tree/tag %))))

(defn find-in
  [node tags]
  (reduce find-tag node tags))

(defn find-child-in
  [node tags]
  (some-> (find-in node tags)
          (children)
          (first)))

(defn find-child-node-in
  [node tags]
  (when-let [children (some-> (find-in node tags)
                              (children))]
    (first (filter branch? children))))

(defn find-children-in
  [node tags]
  (some-> (find-in node tags)
          (children)))
