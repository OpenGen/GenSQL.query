(ns inferenceql.query.plan.viz
  (:require [clojure.string :as string]
            [clojure.zip :as zip]
            [inferenceql.query.environment :as env]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.plan :as plan]
            [rhizome.viz :as rhizome]))

(defn branch?
  [node]
  (or plan/plan? (sequential? node)))

(defn children
  [node]
  (cond (plan/plan? node) (plan/children node)
        (sequential? node) (rest node)))

(defn child-locs
  [node]
  (when-let [child (zip/down node)]
    (take-while some? (iterate zip/right child))))

(defn zipper
  [node]
  (let [make-node (fn make-node
                    [& _]
                    (throw (ex-info "Not implemented" {:node node :children children})))]
    (zip/zipper branch? children make-node node)))

(defn node->label
  [loc]
  (let [plan (zip/node loc)]
    (cond (plan/plan? plan) (case (plan/type plan)
                              :inferenceql.query.plan.type/lookup (::env/name plan)
                              (keyword (name (plan/type plan))))
          (vector? plan) (name (first plan))
          (seq? plan) (first plan)
          (symbol? plan) plan
          (keyword? plan) (string/join \space ["VARIABLE" (name plan)])
          (map? plan) plan
          (number? plan) plan
          :else (throw (ex-info "No label for plan" {:plan plan})))))

(defn edge->style
  [loc1 loc2]
  (let [node1 (zip/node loc1)
        node2 (zip/node loc2)]
    (if (and (plan/plan? node1)
             (plan/plan? node2))
      "solid"
      "dotted")))

(defn edge->dir
  [loc1 loc2]
  (let [node1 (zip/node loc1)
        node2 (zip/node loc2)]
    (if (and (plan/plan? node1)
             (plan/plan? node2))
      "back"
      "none")))

(defn edge->descriptor
  [loc1 loc2]
  {:dir (edge->dir loc1 loc2)
   :style (edge->style loc1 loc2)})

(defn node->shape
  [loc]
  (if (plan/plan? (zip/node loc))
    "box"
    "ellipse"))

(defn node->descriptor
  [loc]
  (when-let [label (node->label loc)]
    {:shape (node->shape loc)
     :label label}))

(defn node->graph
  [node]
  (let [zipper (zipper node)
        nodes (take-while (complement zip/end?) (iterate zip/next zipper))]
    (zipmap nodes (map child-locs nodes))))

(defn query->graph
  [s]
  (-> s
      (parser/parse)
      (plan/plan)
      (node->graph)))

(defn node->cluster
  [loc]
  (if (plan/plan? (zip/node loc))
    nil
    (->> (iterate zip/up loc)
         (take-while some?)
         (take-while (complement (comp plan/plan? zip/node)))
         (last))))

(defn viz
  [s]
  (let [graph (query->graph s)]
    (rhizome/view-graph (keys graph) graph
                        :node->descriptor node->descriptor
                        :edge->descriptor edge->descriptor
                        ;; :node->cluster node->cluster
                        :options {:dpi 100})))
