(ns inferenceql.query.plan
  (:refer-clojure :exclude [alias alter distinct distinct? eval sort type update])
  (:require [clojure.core :as core]
            [clojure.core.match :as match]
            [clojure.math.combinatorics :as combinatorics]
            [clojure.set :as set]
            [clojure.string :as string]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.environment :as env]
            [inferenceql.query.literal :as literal]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.scalar :as scalar]
            [inferenceql.query.strict.parser :as parser]
            [inferenceql.query.string :as query.string]
            [inferenceql.query.tuple :as tuple]
            [inferenceql.query.xforms :as query.xforms]
            [net.cgrand.xforms :as xforms]))

(defn relation-node?
  [node]
  (tree/match node
    [:query [:relation-expr & _]] true
    :else false))

(defn plan?
  [x]
  (and (associative? x)
       (contains? x ::type)))

;;; parse tree

(defn type
  [node]
  (get node ::type))

(def eval-literal-in (comp literal/read tree/get-node-in))

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

(defn cross-join
  [op1 op2]
  {::type :inferenceql.query.plan.type/cross-join
   ::plan-1 op1
   ::plan-2 op2})

(defn inner-join
  [op1 op2 condition]
  {::type :inferenceql.query.plan.type/inner-join
   ::plan-1 op1
   ::plan-2 op2
   ::condition condition})

(defn generative-join
  [op sexpr]
  {::type :inferenceql.query.plan.type/generative-join
   ::plan op
   ::sexpr sexpr})

(defn with
  [bindings op]
  {::type :inferenceql.query.plan.type/with
   ::bindings bindings
   ::plan op})

(defn rename
  [op name]
  {::type :inferenceql.query.plan.type/rename
   ::plan op
   ::relation/name name})

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

(defmethod plan-impl :statement
  [node]
  (plan-impl (tree/only-child-node node)))

(defmethod plan-impl :relation-expr
  [node]
  (plan-impl (tree/only-child-node node)))

(defmethod plan-impl nil
  [_ op]
  op)

(defmethod plan-impl :identifier
  [node]
  (let [sym (literal/read node)]
    (lookup sym)))

(defn variable-node->symbol
  [node]
  (-> node tree/only-child-node literal/read ))

(defmethod plan-impl :generate-expr
  [node]
  (tree/match [node]
    [[:generate-expr _generate generate-list _under model-expr]]
    (let [sexpr (scalar/plan model-expr)
          variables (case (tree/tag generate-list)
                      :star
                      '*

                      :variable-list
                      (map variable-node->symbol (tree/child-nodes generate-list)))]
      (generate variables sexpr))))

(defmethod plan-impl :where-clause
  [node op]
  (let [sexpr (-> (tree/get-node node :scalar-expr)
                  (scalar/plan))]
    (select op sexpr)))

(defn input-attr
  [node]
  (tree/match [node]
    [[:selection child & _]] (input-attr child)
    [[:aggregation _aggregator "(" [:star & _] ")"]] nil
    [[:aggregation _aggregator "(" _distinct [:star & _] ")"]] nil
    [[:aggregation _aggregator "(" child ")"]] (literal/read child)
    [[:aggregation _aggregator "(" _distinct child ")"]] (literal/read child)
    [[:scalar-expr child]] (literal/read child)))

(defn output-attr
  "For a selection returns the attribute for that selection in the output
  relation."
  [node]
  (tree/match [node]
              [[:selection "(" child ")"]] (output-attr child)
              [[:selection _ [:alias-clause _as sym-node]]] (literal/read sym-node)
              [[:selection child]] (-> (parser/unparse child)
                                       (query.string/safe-symbol))))

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
    [[:aggregation-fn [tag & _]]] (symbol tag)              ; No need to munge agg fns.
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
    [[:scalar-expr [:identifier _]]] true
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
  (tree/match [node]
    [[:from-clause _from relation-expr]]
    (plan-impl relation-expr)))

(defmethod plan-impl :rename-expr
  [node]
  (tree/match [node]
    [[:rename-expr relation-expr [:alias-clause _as identifier]]]
    (rename (plan relation-expr) (literal/read identifier))))

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
    [[_id [:simple-symbol s]]]           (sort op (symbol s) :ascending)
    [[_id [:simple-symbol s]] [:asc  _]] (sort op (symbol s) :ascending)
    [[_id [:simple-symbol s]] [:desc _]] (sort op (symbol s) :descending)
    [[_id [:delimited-symbol s]]]           (sort op (query.string/safe-symbol s) :ascending)
    [[_id [:delimited-symbol s]] [:asc  _]] (sort op (query.string/safe-symbol s) :ascending)
    [[_id [:delimited-symbol s]] [:desc _]] (sort op (query.string/safe-symbol s) :descending)))

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

(defmethod plan-impl :join-expr
  [node]
  (plan-impl (tree/only-child-node node)))

(defmethod plan-impl :join-expr-group
  [node]
  (plan-impl (tree/only-child-node node)))

(defmethod plan-impl :cross-join-expr
  [node]
  (tree/match [node]
    [[:cross-join-expr rel-node-1 _cross _join rel-node-2]]
    (cross-join (plan rel-node-1)
                (plan rel-node-2))))

(defmethod plan-impl :inner-join-expr
  [node]
  (tree/match [node]
    [[:inner-join-expr rel-node-1 _join rel-node-2 _on scalar-expr]]
    (inner-join (plan rel-node-1)
                (plan rel-node-2)
                (scalar/plan scalar-expr))

    [[:inner-join-expr rel-node-1 _inner _join rel-node-2 _on scalar-expr]]
    (inner-join (plan rel-node-1)
                (plan rel-node-2)
                (scalar/plan scalar-expr))))

(defmethod plan-impl :generative-join-expr
  [node]
  (tree/match [node]
    [[:generative-join-expr rel-node _generative _join model-expr]]
    (generative-join (plan rel-node)
                     (scalar/plan model-expr))))

(defmethod plan-impl :with-expr
  [node]
  (let [expr-plan (fn expr-plan [node]
                    (case (tree/tag node)
                      :relation-expr (plan node)
                      :scalar-expr (scalar/plan node)
                      :model-expr (scalar/plan node)
                      (recur (first (tree/child-nodes node)))))
        children (tree/child-nodes node)
        binding-nodes (butlast children)
        relation-node (last children)
        plan (plan relation-node)]
    (with (into []
                (map (juxt tree/alias expr-plan))
                binding-nodes)
          plan)))

;;; eval

(defmulti eval
  "Evaluates a relational query plan in the context of an environment. Returns a
  relation."
  (fn eval-dispatch
    [plan _env _bindings]
    (when (nil? plan)
      (throw (ex-info "Query plan is nil" {})))
    (::type plan)))

(defmethod eval :inferenceql.query.plan.type/lookup
  [plan env bindings]
  (let [{::env/keys [name]} plan
        rel (env/safe-get env bindings name)]
    (if (relation/relation? rel)
      (relation/assoc-name rel name)
      (relation/relation rel :name name))))

(defmethod eval :inferenceql.query.plan.type/select
  [plan env bindings]
  (let [{::keys [sexpr plan]} plan
        rel (eval plan env bindings)
        pred #(scalar/eval sexpr env bindings %)]
    (relation/select rel pred)))

(defmethod eval :inferenceql.query.plan.type/extended-project
  [plan env bindings]
  (let [{::keys [terms plan]} plan
        f-attr-pairs (map (juxt (fn [{::keys [sexpr]}]
                                  (fn [tuple]
                                    (scalar/eval sexpr env bindings tuple)))
                                ::relation/attribute)
                          terms)
        rel (eval plan env bindings)]
    (relation/extended-project rel f-attr-pairs)))

(defmethod eval :inferenceql.query.plan.type/limit
  [plan env bindings]
  (let [{::keys [limit plan]} plan
        rel (eval plan env bindings)]
    (relation/limit rel limit)))

(defmethod eval :inferenceql.query.plan.type/distinct
  [plan env bindings]
  (let [{::keys [plan]} plan
        rel (eval plan env bindings)]
    (relation/distinct rel)))

(defmethod eval :inferenceql.query.plan.type/sort
  [plan env bindings]
  (let [{::keys [plan order] ::relation/keys [attribute]} plan
        rel (eval plan env bindings)]
    (relation/sort rel attribute order)))

(defmethod eval :inferenceql.query.plan.type/generate
  [plan env bindings]
  (let [{::keys [sexpr variables]} plan
        model (scalar/eval sexpr env bindings)
        variables (->> (if (= '* variables)
                         (gpm/variables model)
                         variables)
                       (map keyword))
        attrs (map query.string/safe-symbol variables)
        samples (map #(update-keys % query.string/safe-symbol)
                     (repeatedly #(gpm/simulate model variables {})))]
    (relation/relation samples :attrs attrs)))

(defmethod eval :inferenceql.query.plan.type/insert
  [plan env bindings]
  (let [plan-to (::plan plan)
        plan-from (::plan-from plan)
        rel-to (eval plan-to env bindings)
        rel-from (eval plan-from env bindings)
        attributes (relation/attributes rel-to)
        rel (into (vec rel-to) rel-from)]
    (relation/relation rel :attrs attributes)))

(def ^:private agg-f
  (-> {'count xforms/count
       'avg xforms/avg
       'sum (xforms/reduce +)
       'median query.xforms/median
       'std xforms/sd
       'max xforms/max
       'min xforms/min}
      (update-vals #(comp (remove nil?) %))))

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
  [plan env bindings]
  (let [{::keys [plan groups aggregations]} plan
        grouping-f (if (seq groups)
                     #(select-keys (tuple/->map %) groups)
                     (constantly nil))
        rel (eval plan env bindings)
        groups (relation/group-by rel grouping-f)
        attributes (map ::output-attr aggregations)]
    (relation/relation (map #(zipmap attributes %)
                            (if (seq groups)
                              (map #(aggregate aggregations %) groups)
                              (default-aggregation aggregations)))
                       :attrs attributes)))

(defn ^:private setting->f
  [[attr sexpr] where-sexpr env bindings]
  (fn [tuple]
    (cond-> tuple
      (scalar/eval where-sexpr env bindings tuple)
      (assoc attr (scalar/eval sexpr env bindings tuple)))))

(defmethod eval :inferenceql.query.plan.type/update
  [plan env bindings]
  (let [{::keys [plan settings sexpr]} plan
        sexpr (or sexpr true)
        rel (eval plan env bindings)
        f (reduce comp (map #(setting->f % sexpr env bindings)
                            settings))
        xf (map f)]
    (relation/transduce rel xf)))

(defmethod eval :inferenceql.query.plan.type/alter
  [plan env bindings]
  (let [{::keys [plan] ::relation/keys [attribute]} plan
        rel (eval plan env bindings)]
    (relation/add-attribute rel attribute)))

(defmethod eval :inferenceql.query.plan.type/rename
  [plan env bindings]
  (let [{::keys [plan] ::relation/keys [name]} plan
        rel (eval plan env bindings)]
    (relation/assoc-name rel name)))

(defmethod eval :inferenceql.query.plan.type/value
  [plan _env _bindings]
  (::relation/relation plan))

(defmethod eval :inferenceql.query.plan.type/cross-join
  [plan env bindings]
  (let [{::keys [plan-1 plan-2]} plan
        rel-1 (eval plan-1 env bindings)
        rel-2 (eval plan-2 env bindings)
        attrs-rel-1 (apply sorted-set (relation/attributes rel-1))
        attrs-rel-2 (apply sorted-set (relation/attributes rel-2))
        attrs-both (set/intersection attrs-rel-1 attrs-rel-2)
        attr-name (fn [name attr]
                    (if (contains? attrs-both attr)
                      (symbol (str name "." attr))          ; MD: NOT namespaced as is
                      attr))
        attrs (vec
               (concat (map #(attr-name (relation/name rel-1) %)
                            attrs-rel-1)
                       (map #(attr-name (relation/name rel-2) %)
                            attrs-rel-2)))
        tuples (sequence (map (fn [[tup1 tup2]]
                                (tuple/tuple (merge (update-keys tup1 #(attr-name (tuple/name tup1) %))
                                                    (update-keys tup2 #(attr-name (tuple/name tup2) %)))
                                             :attributes attrs)))
                         (combinatorics/cartesian-product (relation/tuples rel-1)
                                                          (relation/tuples rel-2)))]
    (relation/relation tuples :attrs attrs)))

(defmethod eval :inferenceql.query.plan.type/inner-join
  [plan env bindings]
  (let [{::keys [plan-1 plan-2 condition]} plan
        rel-1 (eval plan-1 env bindings)
        rel-2 (eval plan-2 env bindings)
        attrs (into []
                    (core/distinct)
                    (into (relation/attributes rel-1)
                          (relation/attributes rel-2)))
        tuples (sequence (comp (filter #(apply scalar/eval condition env bindings %))
                               (map #(apply merge %)))
                         (combinatorics/cartesian-product (relation/tuples rel-1)
                                                          (relation/tuples rel-2)))]
    (relation/relation tuples :attrs attrs)))

(defmethod eval :inferenceql.query.plan.type/generative-join
  [plan env bindings]
  (let [{::keys [plan sexpr]} plan
        rel (eval plan env bindings)
        tuples (sequence (map (fn [row]
                                (let [model (scalar/eval sexpr env bindings row)
                                      variables (gpm/variables model)
                                      extension (update-keys (gpm/simulate model variables {})
                                                             symbol)]
                                  (merge row extension))))
                         (relation/tuples rel))]
    (relation/relation tuples)))

(defmethod eval :inferenceql.query.plan.type/with
  [plan env bindings]
  (let [{::keys [plan] binding-plans ::bindings} plan
        bindings (loop [bindings bindings
                        binding-plans binding-plans]
                   (if-let [[attr plan] (first binding-plans)]
                     (let [v (if (plan? plan)
                               (eval plan env bindings)
                               (scalar/eval plan env bindings))]
                       (recur (assoc bindings attr v)
                              (rest binding-plans)))
                     bindings))]
    (eval plan env bindings)))

(defmethod eval :inferenceql.query.plan.type/with
  [plan env bindings]
  (let [{::keys [plan] binding-plans ::bindings} plan
        bindings (loop [bindings bindings
                        binding-plans binding-plans]
                   (if-let [[attr plan] (first binding-plans)]
                     (let [v (if (plan? plan)
                               (eval plan env bindings)
                               (scalar/eval plan env bindings))]
                       (recur (assoc bindings attr v)
                              (rest binding-plans)))
                     bindings))]
    (eval plan env bindings)))
