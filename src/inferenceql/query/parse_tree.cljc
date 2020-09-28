(ns inferenceql.query.parse-tree
  (:require [clojure.string :as string]
            [inferenceql.query.node :as node]))

(defn branch?
  "Returns true if `node` could have children (but may not)."
  [node]
  (vector? node))

(defn node
  "Creates a new node."
  [tag children]
  (into [tag] children))

(defn tag
  "Returns the tag, or node type, of `node`."
  [node]
  (when (branch? node)
    (first node)))

(defn whitespace?
  "Returns `true` if `node` is a whitespace node. `false` otherwise."
  [node]
  (or (and (branch? node)
           (= :ws (tag node)))
      (and (string? node)
           (string/blank? node))))

(defn children
  "Returns `node`'s children."
  [node]
  (into []
        (remove whitespace?)
        (rest node)))

(defn child-nodes
  "Returns the children of `node` that are themselves nodes."
  [node]
  (filter branch? (children node)))

(defn only-child
  "Returns the only child of `node`. Throws if `node` does not have one child."
  [node]
  (let [children (children node)]
    (if-not (= 1 (count children))
      (throw (ex-info (str "Expected one child, but found " (count children))
                      {:node node}))
      (first children))))

(defn get-node
  "Returns the first child of `node` with tag `k` if `k` is a keyword, or returns
  the `k`th child if `k` is a natural number."
  ([node k]
   (get-node node k nil))
  ([node k not-found]
   (let [children (child-nodes node)]
     (cond (keyword? k) (nth (filter #(node/has-tag? % k) children)
                             0
                             not-found)
           (nat-int? k) (nth (child-nodes node) k not-found)))))

(defn get-node-in
  "Like `get-in`, but descends via `get-node` instead of `get`."
  ([node ks]
   (get-node-in node ks nil))
  ([node ks not-found]
   (loop [sentinel #?(:clj (Object.)
                      :cljs (js-obj))
          node node
          ks (seq ks)]
     (if ks
       (let [child (get-node node (first ks) sentinel)]
         (if (identical? sentinel child)
           not-found
           (recur sentinel child (next ks))))
       node))))
