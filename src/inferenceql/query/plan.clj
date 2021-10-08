(ns inferenceql.query.plan
  (:refer-clojure :exclude [eval sort])
  (:require [clojure.core.match :as match]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.environment :as env]
            [inferenceql.query.model :as model]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.scalar :as scalar]
            [medley.core :as medley]))

(defmulti plan-spec ::type)

(s/def ::plan (s/multi-spec plan-spec ::type))

(s/def ::sexpr any?)
(s/def ::limit pos-int?)
(s/def ::variables (s/coll-of ::model/variable))

(defmethod plan-spec :inferenceql.query.plan.type/lookup
  [_]
  (s/keys :req [::env/name]))

(defmethod plan-spec :inferenceql.query.plan.type/select
  [_]
  (s/keys :req [::plan ::sexpr]))

(defmethod plan-spec :inferenceql.query.plan.type/extended-project
  [_]
  (s/keys :req [::plan ::terms]))

(s/def ::term (s/keys :req [::sexpr ::relation/attribute]))

(s/def ::terms (s/coll-of ::term))

(defmethod plan-spec :inferenceql.query.plan.type/generate
  [_]
  (s/keys :req [::sexpr ::variables]))

(defn plan?
  [x]
  (s/valid? ::plan x))

;;; parse tree

(defn eval-literal
  ([node]
   (match/match node
     nil nil
     [:null _] nil
     [_ s] (edn/read-string s)))
  ([node tag]
   (when node
     (eval-literal (tree/get-node node tag)))))

(def eval-literal-in (comp eval-literal tree/get-node-in))

;;; plan

(defn lookup
  [sym]
  {::type :inferenceql.query.plan.type/lookup
   ::env/name sym})

(defn select
  [op attrs]
  {::type :inferenceql.query.plan.type/select
   ::sexpr attrs
   ::plan op})

(defn extended-project
  [op coll]
  ;; `coll` is a sequence of (s-expression, attribute) pairs.
  (let [terms (mapv #(zipmap [::sexpr ::relation/attribute] %)
                    coll)]
    {::type :inferenceql.query.plan.type/extended-project
     ::terms terms
     ::plan op}))

(defn generate
  [variables sexpr]
  {::type :inferenceql.query.plan.type/generate
   ::sexpr sexpr
   ::variables variables})

(defn limit
  [op limit]
  {::type :inferenceql.query.plan.type/limit
   ::limit limit
   ::plan op})

(defn sort
  [op attr order]
  {::type :inferenceql.query.plan.type/sort
   ::relation/attribute attr
   ::order order
   ::plan op})

;;; plan

(defmulti plan
  "Returns a query plan for the provided parse tree node. The 2-arity version is
  used when the plan that provides the input relation is generated from a node
  elsewhere in the parse tree, as is the case with `\"SELECT\"` subclauses."
  (fn
    ([node] (tree/tag node))
    ([node _op] (tree/tag node))))

(comment

 (parser/parse "SELECT * FROM data WHERE x=0 AND (x=1 OR x=1)")
 (plan (parser/parse "SELECT * FROM data WHERE x=0 AND (x=1 OR x=1)"))

 ,)

(defmethod plan :default
  ([node]
   (plan (tree/only-child-node node)))
  ([node op]
   (plan (tree/only-child-node node) op)))

(defmethod plan nil
  [_ op]
  op)

(defmethod plan :simple-symbol
  [node]
  (let [sym (eval-literal node)]
    (lookup sym)))

(defn variable-node->symbol
  [node]
  (-> node tree/only-child-node eval-literal))

(defmethod plan :generate-expr
  [node]
  (let [sexpr (scalar/plan (tree/get-node node :model-expr))
        variables (let [generate-list-nodes (tree/child-nodes (tree/get-node-in node [:generate-clause :generate-list]))]
                    (if (tree/star? (first generate-list-nodes))
                      '*
                      (map variable-node->symbol generate-list-nodes)))]
    (generate variables sexpr)))

(defmethod plan :where-clause
  [node op]
  (let [sexpr (-> (tree/get-node node :scalar-expr)
                  (scalar/plan))]
    (select op sexpr)))

(defn output-attribute
  [node]
  (assert (= :selection (tree/tag node)))
  (if-let [expr (tree/get-node node :scalar-expr)]
    (or (eval-literal-in node [:alias-clause :simple-symbol])
        (-> (parser/unparse expr)
            (string/replace #"\s" "")
            (symbol)))
    (recur (tree/get-node node :selection))))

(defmethod plan :select-clause
  [node op]
  (if (tree/star? node)
    op
    (let [selection->pair (fn [node]
                            (assert (= :selection (tree/tag node)))
                            (if-let [expr (tree/get-node node :scalar-expr)]
                              (let [sexpr (scalar/plan expr)
                                    output-attribute (output-attribute node)]
                                [sexpr output-attribute])
                              (recur (tree/only-child-node node))))
          projections (map selection->pair (tree/child-nodes (tree/get-node node :select-list)))]
      (extended-project op projections))))

(defmethod plan :limit-clause
  [node op]
  (let [n (eval-literal-in node [:int])]
    (limit op n)))

(defmethod plan :order-by-clause
  [node op]
  (match/match (vec (tree/child-nodes node))
    [[:simple-symbol s]]           (sort op (symbol s) :ascending)
    [[:simple-symbol s] [:asc  _]] (sort op (symbol s) :ascending)
    [[:simple-symbol s] [:desc _]] (sort op (symbol s) :descending)))

(defmethod plan :select-expr
  [node]
  (->> (plan (tree/get-node node :from-clause))
       (plan (tree/get-node node :where-clause))
       (plan (tree/get-node node :select-clause))
       (plan (tree/get-node node :order-by-clause))
       (plan (tree/get-node node :limit-clause))))

;;; eval

(defmulti eval
  "Evaluates a relational query plan in the context of an environment. Returns a
  relation."
  (fn [plan _env] (::type plan)))

(defmethod eval :inferenceql.query.plan.type/lookup
  [plan env]
  (let [{::env/keys [name]} plan]
    (env/get env name)))

(defmethod eval :inferenceql.query.plan.type/select
  [plan env]
  (let [{::keys [sexpr plan]} plan
        rel (eval plan env)
        pred #(scalar/eval sexpr env %)]
    (relation/select rel pred)))

(defmethod eval :inferenceql.query.plan.type/extended-project
  [plan env]
  (let [{::keys [terms plan]} plan
        coll (map (juxt (fn [{::keys [sexpr]}]
                          (fn [tuple]
                            (scalar/eval sexpr env tuple)))
                        ::relation/attribute)
                  terms)
        rel (eval plan env)]
    (relation/extended-project rel coll)))

(comment

 (plan (parser/parse "SELECT x FROM data"))

 ,)

(defmethod eval :inferenceql.query.plan.type/limit
  [plan env]
  (let [{::keys [limit plan]} plan
        rel (eval plan env)]
    (relation/limit rel limit)))

(defmethod eval :inferenceql.query.plan.type/sort
  [plan env]
  (let [{::keys [plan order] ::relation/keys [attribute]} plan
        rel (eval plan env)]
    (relation/sort rel attribute order)))

(defmethod eval :inferenceql.query.plan.type/generate
  [plan env]
  (let [{::keys [sexpr variables]} plan
        model (scalar/eval sexpr env)
        variables (->> (if (= '* variables)
                         (gpm/variables model)
                         variables)
                       (map keyword))
        samples (map #(medley/map-keys symbol %)
                     (repeatedly #(gpm/simulate model variables {})))]
    (relation/relation samples variables)))
