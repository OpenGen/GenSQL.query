(ns inferenceql.query.plan
  (:refer-clojure :exclude [eval sort update])
  (:require [clojure.core.match :as match]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.environment :as env]
            [inferenceql.query.literal :as literal]
            [inferenceql.query.model :as model]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.scalar :as scalar]
            [medley.core :as medley]))

(defmulti plan-spec ::type)

(s/def ::plan (s/multi-spec plan-spec ::type))

(s/def ::plan-to ::plan)
(s/def ::plan-from ::plan)

(s/def ::sexpr any?)
(s/def ::limit pos-int?)
(s/def ::variables (s/coll-of ::model/variable))
(s/def ::settings (s/map-of ::relation/attribute ::sexpr))

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

(defmethod plan-spec :inferenceql.query.plan.type/insert
  [_]
  (s/keys :req [::plan-to ::plan-from]))

(defmethod plan-spec :inferenceql.query.plan.type/value
  [_]
  (s/keys :req [::relation/relation]))

(defmethod plan-spec :inferenceql.query.plan.type/update
  [_]
  (s/keys :req [::settings]))

(defmethod plan-spec :inferenceql.query.plan.type/alter
  [_]
  (s/keys :req [::plan ::relation/attribute]))

(defn plan?
  [x]
  (s/valid? ::plan x))

;;; parse tree

(defn eval-literal
  ([node]
   (match/match node
     nil nil
     [:null _] nil
     [:value child] (recur child)
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
  [op sexpr]
  {::type :inferenceql.query.plan.type/select
   ::sexpr sexpr
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

(defn insert
  [op1 op2]
  {::type :inferenceql.query.plan.type/insert
   ::plan-to op1
   ::plan-from op2})

(defn value
  [rel]
  {::type :inferenceql.query.plan.type/value
   ::relation/relation rel})

(defn update
  ([op settings]
   {::type :inferenceql.query.plan.type/update
    ::plan op
    ::settings settings})
  ([op settings sexpr]
   (assoc (update op settings)
          ::sexpr sexpr)))

(defn alter
  [op attr]
  {::type :inferenceql.query.plan.type/alter
   ::plan op
   ::relation/attribute attr})

;;; plan

(defmulti plan
  "Returns a query plan for the provided parse tree node. The 2-arity version is
  used when the plan that provides the input relation is generated from a node
  elsewhere in the parse tree, as is the case with `\"SELECT\"` subclauses."
  (fn
    ([node] (tree/tag node))
    ([node _op] (tree/tag node))))

(defmethod plan :query
  [node]
  (plan (tree/only-child-node node)))

(defmethod plan :relation-expr
  [node]
  (plan (tree/only-child-node node)))

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

(defmethod plan :from-clause
  [node]
  (plan (tree/only-child-node node)))

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

(defmethod plan :insert-expr
  [node]
  (let [[node1 node2] (tree/child-nodes node)]
    (insert (plan node1) (plan node2))))

(defmethod plan :update-expr
  [node]
  (let [settings->map (fn [node]
                        (match/match node
                          [:update-setting symbol-node _= scalar-node]
                          (let [sym (literal/read symbol-node)
                                plan (scalar/plan scalar-node)]
                            {sym plan})))
        [rel-node settings-node where-node] (tree/child-nodes node)
        rel-plan (plan rel-node)
        settings (into {} (map settings->map (tree/child-nodes settings-node)))]
    (if where-node
      (let [where-plan (scalar/plan where-node)]
        (update rel-plan settings where-plan))
      (update rel-plan settings))))

(defmethod plan :alter-expr
  [node]
  (let [[rel-node attr-node] (tree/child-nodes node)
        rel-plan (plan rel-node)
        attr (literal/read attr-node)]
    (alter rel-plan attr)))

(defmethod plan :relation-value
  [node]
  (let [rel (literal/read node)]
    (value rel )))

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

(defmethod eval :inferenceql.query.plan.type/insert
  [plan env]
  (let [plan-to (::plan-to plan)
        plan-from (::plan-from plan)
        rel-to (eval plan-to env)
        rel-from (eval plan-from env)]
    (relation/relation (into (vec rel-to) rel-from) (relation/attributes rel-to))))

(defn ^:private setting->f
  [[attr sexpr] where-sexpr env]
  (fn [tuple]
    (cond-> tuple
      (scalar/eval where-sexpr env tuple)
      (assoc attr (scalar/eval sexpr env tuple)))))

(defmethod eval :inferenceql.query.plan.type/update
  [plan env]
  (let [{::keys [plan settings sexpr]} plan
        sexpr (or sexpr true)
        rel (eval plan env)
        f (reduce comp (map #(setting->f % sexpr env)
                            settings))
        xf (map f)]
    (relation/transduce rel xf)))

(defmethod eval :inferenceql.query.plan.type/alter
  [plan env]
  (let [{::keys [plan] ::relation/keys [attribute]} plan
        rel (eval plan env)]
    (relation/add-attribute rel attribute)))

(defmethod eval :inferenceql.query.plan.type/value
  [plan _]
  (::relation/relation plan))
