(ns inferenceql.query.plan
  (:refer-clojure :exclude [alias alter distinct distinct? eval sort type update])
  (:require [clojure.core :as core]
            [clojure.core.match :as match]
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
            [inferenceql.query.tuple :as tuple]
            [inferenceql.query.xforms :as query.xforms]
            [medley.core :as medley]
            [net.cgrand.xforms :as xforms]))

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
  (s/keys :req [::plan ::plan-from]))

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

(defn type
  [node]
  (get node ::type))

(def eval-literal-in (comp eval-literal tree/get-node-in))

(defn children
  [node]
  (let [{::keys [sexpr plan plan-from terms variables]} node]
    (remove nil? (into [plan plan-from sexpr]
                       (concat (map ::sexpr terms)
                               variables)))))

(defn star?
  [node]
  (tree/match [node]
    [[:selection child]] (star? child)
    [[:star & _]] true
    :else false))

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

(defn distinct
  [op]
  {::type :inferenceql.query.plan.type/distinct
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
   ::plan op1
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

(s/def ::output-attribute ::relation/attribute)
(s/def ::input-attribute ::relation/attribute)
(s/def ::aggregator ::env/sym)

(s/def ::groups (s/coll-of ::sexpr))
(s/def ::aggregation (s/keys :req [::aggregator ::input-attr ::output-attr]))
(s/def ::aggregations (s/coll-of ::aggregation))

(defmethod plan-spec :inferenceql.query.plan.type/group
  [_]
  (s/keys :req [::plan ::aggregations] :opt [::groups]))

(defn grouping
  ([op aggregations]
   (grouping op aggregations '()))
  ([op aggregations groups]
   (cond-> {::type :inferenceql.query.plan.type/group
            ::aggregations aggregations
            ::plan op}
     (seq groups) (assoc ::groups groups))))

;;; plan

(defmulti plan-impl
  "Returns a query plan for the provided parse tree node. The 2-arity version is
  used when the plan that provides the input relation is generated from a node
  elsewhere in the parse tree, as is the case with \"SELECT\" subclauses."
  (fn
    ([node] (tree/tag node))
    ([node _op] (tree/tag node))))

(defn plan
  ([node]
   (with-meta (plan-impl node)
     {::parser/node node}))
  ([node op]
   (if (some? node)
     (with-meta (plan-impl node op)
       {::parser/node node})
     op)))

(defn node
  [plan]
  (get (meta plan) ::parser/node))

(defmethod plan-impl :query
  [node]
  (plan-impl (tree/only-child-node node)))

(defmethod plan-impl :relation-expr
  [node]
  (plan-impl (tree/only-child-node node)))

(defmethod plan-impl nil
  [_ op]
  op)

(defmethod plan-impl :simple-symbol
  [node]
  (let [sym (eval-literal node)]
    (lookup sym)))

(defn variable-node->symbol
  [node]
  (-> node tree/only-child-node eval-literal))

(defmethod plan-impl :generate-expr
  [node]
  (let [sexpr (scalar/plan (tree/get-node node :model-expr))
        variables (let [generate-list-nodes (tree/child-nodes (tree/get-node-in node [:generate-clause :generate-list]))]
                    (if (star? (first generate-list-nodes))
                      '*
                      (map variable-node->symbol generate-list-nodes)))]
    (generate variables sexpr)))

(defmethod plan-impl :where-clause
  [node op]
  (let [sexpr (-> (tree/get-node node :scalar-expr)
                  (scalar/plan))]
    (select op sexpr)))

(defn input-attr
  [node]
  (tree/match [node]
    [[:selection child & _]] (input-attr child)
    [[:selection-group "(" child ")"]] (input-attr child)
    [[:aggregation _aggregator "(" [:star & _] ")"]] nil
    [[:aggregation _aggregator "(" _distinct [:star & _] ")"]] nil
    [[:aggregation _aggregator "(" child ")"]] (eval-literal child)
    [[:aggregation _aggregator "(" _distinct child ")"]] (eval-literal child)
    [[:scalar-expr child]] (eval-literal child)))

(defn output-attr
  "For a selection returns the attribute for that selection in the output
  relation."
  [node]
  (tree/match [node]
    [[:selection "(" child ")"]] (output-attr child)
    [[:selection _ [:alias-clause _as sym-node]]] (eval-literal sym-node)
    [[:selection child]] (-> (parser/unparse child)
                             (string/replace #"\s" "")
                             (symbol))))

(defn ^:private selection-plan
  [node op]
  (let [selection->pair (fn [node]
                          (assert (= :selection (tree/tag node)))
                          (if-let [expr (tree/get-node node :scalar-expr)]
                            (let [sexpr (scalar/plan expr)
                                  attr (output-attr node)]
                              [sexpr attr])
                            (recur (tree/only-child-node node))))
        projections (map selection->pair (tree/child-nodes (tree/get-node node :select-list)))]
    (extended-project op projections)))

(defn aggregator
  [node]
  (tree/match [node]
    [[:selection child & _]] (aggregator child)
    [[:aggregation aggregation-fn "(" _sym ")"]] (aggregator aggregation-fn)
    [[:aggregation aggregation-fn "(" _distinct _sym ")"]] (aggregator aggregation-fn)
    [[:aggregation-fn [tag & _]]] (symbol tag)
    [[:scalar-expr _]] nil))

(defn selections
  [node]
  (case (tree/tag node)
    :select-expr (tree/get-child-nodes-in node [:select-clause :select-list])
    :select-clause (tree/get-child-nodes-in node [:select-list])
    :select-list (tree/child-nodes node)))

(defn distinct?
  [node]
  (tree/match [node]
    [[:selection child & _]] (distinct? child)
    [[:aggregation _fn _o-paren _sym _c-paren]] false
    [[:aggregation _fn _o-paren _distinct _sym _c-paren]] true
    :else false))

(defn ^:private aggregation
  [node]
  (let [input-attr (input-attr node)
        output-attr (output-attr node)
        aggregator (aggregator node)]
    (cond-> {}
      input-attr (assoc ::input-attr input-attr)
      output-attr (assoc ::output-attr output-attr)
      aggregator (assoc ::aggregator aggregator)
      (distinct? node) (assoc ::distinct true))))

(defn aggregation?
  [node]
  (tree/match [node]
    [[:selection child & _]] (aggregation? child)
    [[:aggregation & _]] true
    :else false))

(defn column-selection?
  [node]
  (tree/match [node]
    [[:selection child & _]] (column-selection? child)
    [[:scalar-expr [:simple-symbol _]]] true
    :else false))

(defn ^:private aggregation-plan
  [selection-node group-by-node op]
  (let [selections (selections selection-node)]
    (assert (every? (some-fn aggregation? column-selection?)
                    selections))
    (grouping op
              (map aggregation selections)
              (map scalar/plan (tree/child-nodes group-by-node)))))

(defn scalar-expr?
  [node]
  (match/match [node]
    [[:selection child & _]] (recur child)
    [[:scalar-expr & _]] true
    :else false))

(defn select-plan
  [select-node group-by-node op]
  (assert (= :select-clause (tree/tag select-node)))
  (assert (or (nil? group-by-node) (= :group-by-clause (tree/tag group-by-node))))
  (let [selections (selections select-node)
        distinct-clause (tree/get-node select-node :distinct-clause)
        plan (cond (and (= 1 (count selections))
                        (star? (first selections)))
                   op

                   (or group-by-node (some aggregation? selections))
                   (aggregation-plan select-node group-by-node op)

                   (every? scalar-expr? selections)
                   (selection-plan select-node op)

                   :else
                   (throw (ex-info "Can't generate selection plan"
                                   {:select-node select-node
                                    :group-by-node group-by-node})))]
    (cond-> plan distinct-clause (distinct))))

(defmethod plan-impl :from-clause
  [node]
  (plan-impl (tree/only-child-node node)))

(defmethod plan-impl :limit-clause
  [node op]
  (let [n (eval-literal-in node [:int])]
    (limit op n)))

(defmethod plan-impl :distinct-clause
  [_ op]
  (distinct op))

(defmethod plan-impl :order-by-clause
  [node op]
  (match/match (vec (tree/child-nodes node))
    [[:simple-symbol s]]           (sort op (symbol s) :ascending)
    [[:simple-symbol s] [:asc  _]] (sort op (symbol s) :ascending)
    [[:simple-symbol s] [:desc _]] (sort op (symbol s) :descending)))

(defmethod plan-impl :select-expr
  [node]
  (->> (plan-impl (tree/get-node node :from-clause))
       (plan-impl (tree/get-node node :where-clause))
       (select-plan (tree/get-node node :select-clause)
                    (tree/get-node node :group-by-clause))
       (plan-impl (tree/get-node node :order-by-clause))
       (plan-impl (tree/get-node node :limit-clause))))

(defmethod plan-impl :insert-expr
  [node]
  (let [[node1 node2] (tree/child-nodes node)]
    (insert (plan node1) (plan node2))))

(defmethod plan-impl :update-expr
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

(defmethod plan-impl :alter-expr
  [node]
  (let [[rel-node attr-node] (tree/child-nodes node)
        rel-plan (plan rel-node)
        attr (literal/read attr-node)]
    (alter rel-plan attr)))

(defmethod plan-impl :relation-value
  [node]
  (let [rel (literal/read node)]
    (value rel)))

;;; eval

(defmulti eval
  "Evaluates a relational query plan in the context of an environment. Returns a
  relation."
  (fn [plan _env]
    (when (nil? plan)
      (throw (ex-info "Query plan is nil" {})))
    (::type plan)))

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

(defmethod eval :inferenceql.query.plan.type/distinct
  [plan env]
  (let [{::keys [plan]} plan
        rel (eval plan env)]
    (relation/distinct rel)))

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
  (let [plan-to (::plan plan)
        plan-from (::plan-from plan)
        rel-to (eval plan-to env)
        rel-from (eval plan-from env)
        attributes (relation/attributes rel-to)
        rel (into (vec rel-to) rel-from)]
    (relation/relation rel attributes)))

(def ^:private agg-f
  (->> {'count xforms/count
        'avg xforms/avg
        'median query.xforms/median
        'std xforms/sd
        'max xforms/max
        'min xforms/min}
       (medley/map-vals #(comp (remove nil?) %))))

(defn ^:private aggregation->xform
  [{::keys [aggregator distinct input-attr]}]
  (comp (if input-attr
          (map input-attr)
          (map identity))
        (if distinct
          (core/distinct)
          (map identity))
        (if aggregator
          (agg-f aggregator)
          xforms/last)))

(defn ^:private default-aggregation
  [aggregations]
  [(map #(when (= 'count (::aggregator %))
           0)
        aggregations)])

(defn ^:private aggregate
  "Performs an aggregation on a group."
  [aggregations group]
  (xforms/transjuxt (map aggregation->xform aggregations)
                    group))

(defmethod eval :inferenceql.query.plan.type/group
  [plan env]
  (let [{::keys [plan groups aggregations]} plan
        grouping-f (if (seq groups)
                     #(select-keys (tuple/->map %) groups)
                     (constantly nil))
        rel (eval plan env)
        groups (relation/group-by rel grouping-f)
        attributes (map ::output-attr aggregations)]
    (relation/relation (map #(zipmap attributes %)
                            (if (seq groups)
                              (map #(aggregate aggregations %) groups)
                              (default-aggregation aggregations)))
                       attributes)))

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

(comment

  (require '[inferenceql.query.parser :as parser] :reload)
  (plan (parser/parse "select count(*) from data;"))

  (input-attr [:selection [:aggregation [:aggregation-fn [:count "count"]] "(" [:star "*"] ")"]])

  (require '[inferenceql.query :as query])
  (query/q "select count(*) from data;"
           [])

  ((apply comp [inc nil]) 0)

  ,)