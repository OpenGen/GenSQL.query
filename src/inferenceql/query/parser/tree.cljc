(ns inferenceql.query.parser.tree
  (:refer-clojure :exclude [alias remove])
  #?(:cljs (:require-macros [inferenceql.query.parser.tree]))
  (:require #?(:clj [clojure.core.match :as match]
               :cljs [cljs.core.match :as match])
            [clojure.core :as clojure]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [net.cgrand.macrovich :as macrovich]))

(defn branch?
  "Returns true if `node` could have children (but may not)."
  [node]
  (vector? node))

(defn tag
  "Returns the tag, or node type, of `node`."
  [node]
  (when (branch? node)
    (first node)))

(defn make-node
  "Creates a new node."
  [node children]
  (into [(tag node)] children))

(defn tag-pred
  "Returns a predicate that returns `true` if the tag for `node` is `t`."
  [t]
  (fn tag-predicate
    [node]
    (= t (tag node))))

(defn whitespace?
  "Returns `true` if `node` is a whitespace node. `false` otherwise."
  [node]
  (or (and (branch? node)
           (= :ws (tag node)))
      (and (string? node)
           (string/blank? node))))

(defn children
  "Returns `node`'s children. Removes whitespace nodes."
  [node]
  (into []
        (clojure/remove whitespace?)
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

(defn only-child-node
  "Returns the only child of `node`. Throws if `node` does not have one child
  node."
  [node]
  (let [child-nodes (child-nodes node)]
    (if-not (= 1 (count child-nodes))
      (throw (ex-info (str "Expected one child node, but found " (count child-nodes))
                      {:node node}))
      (first child-nodes))))

(defn get-node
  "Returns the first child of `node` with tag `k` if `k` is a keyword, or returns
  the `k`th child if `k` is a natural number."
  ([node k]
   (get-node node k nil))
  ([node k not-found]
   (when (branch? node)
     (let [children (child-nodes node)]
       (cond (keyword? k) (nth (filter (tag-pred k) children)
                               0
                               not-found)
             (nat-int? k) (nth (child-nodes node)
                               k
                               not-found))))))

(defn get-node-in
  "Like `get-in`, but descends via `get-node` instead of `get`. Will skip multiple
  consecutive instances of each `k` if necessary."
  ([node ks]
   (get-node-in node ks nil))
  ([node ks not-found]
   (loop [sentinel #?(:clj (Object.)
                      :cljs (js-obj))
          node node
          ks (seq ks)]
     (if-not ks
       node
       (let [child (get-node node (first ks) sentinel)]
         (if (identical? sentinel child)
           (let [child (get-node node (tag node) sentinel)]
             (if (identical? sentinel child)
               not-found
               (recur sentinel child ks)))
           (recur sentinel child (next ks))))))))

(def get-child-nodes-in (comp child-nodes get-node-in))

(defn alias
  [node]
  (match/match node
    [:alias-clause _as _ws [_id [(:or :simple-symbol :delimited-symbol) s]]] (str s)
    :else (recur (get-node node :alias-clause))))

(defn remove
  [node pred?]
  (let [remove-ws (fn [node]
                    (if-not (branch? node)
                      node
                      (let [children (clojure/remove pred? (children node))]
                        (make-node node children))))]
    (walk/postwalk remove-ws node)))

(defmacro match
  [vars & clauses]
  (let [match (macrovich/case :clj 'clojure.core.match/match
                              :cljs 'cljs.core.match/match)]
    `(~match (remove ~vars whitespace?) ~@clauses)))
