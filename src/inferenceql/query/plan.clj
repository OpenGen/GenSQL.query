(ns inferenceql.query.plan
  (:refer-clojure :exclude [eval])
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [hashp.core]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.inference.gpm.proto :as proto]
            [inferenceql.query.environment :as env]
            [inferenceql.query.math :as math]
            [inferenceql.query.model :as model]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.tuple :as tuple]
            [instaparse.core :as instaparse]
            [malli.core :as malli]
            [sci.core :as sci]))

;;; parse tree

(defn star?
  "Returns `true` if `node` when unparsed would be `\"*\"`."
  [node]
  (case (tree/tag node)
    :star true
    :select-clause (recur (tree/only-child-node node))
    :select-list (recur (first (tree/children node)))
    false))

(defn eval-literal
  ([node]
   (when node
     (edn/read-string (tree/only-child node))))
  ([node tag]
   (when node
     (eval-literal (tree/get-node node tag)))))

(def eval-literal-in (comp eval-literal tree/get-node-in))

;;; expr->sexpr

(def hierarchy
  (-> (make-hierarchy)
      (derive :expr-disjunction       :expr-infix)
      (derive :expr-conjunction       :expr-infix)
      (derive :expr-gt                :expr-infix)
      (derive :expr-geq               :expr-infix)
      (derive :expr-eq                :expr-infix)
      (derive :expr-leq               :expr-infix)
      (derive :expr-lt                :expr-infix)
      (derive :expr-addition          :expr-infix)
      (derive :expr-subtraction       :expr-infix)
      (derive :expr-multiplication    :expr-infix)
      (derive :expr-division          :expr-infix)
      (derive :expr-not               :expr-prefix)
      (derive :conditioned-by-expr    :expr-infix)

      (derive :expr-null              :expr-suffix)
      (derive :expr-not-null          :expr-suffix)

      (derive :distribution-event-and :event-infix)
      (derive :distribution-event-or  :event-infix)
      (derive :distribution-event-gt  :event-infix)
      (derive :distribution-event-geq :event-infix)
      (derive :distribution-event-eq  :event-infix)
      (derive :distribution-event-leq :event-infix)
      (derive :distribution-event-lt  :event-infix)))

(defn infix-operator
  [node]
  (case (tree/tag node)
    :expr-disjunction      'or
    :expr-conjunction      'and
    :expr-gt               '>
    :expr-geq              '>=
    :expr-eq               '=
    :expr-leq              '<=
    :expr-lt               '<
    :expr-addition         '+
    :expr-subtraction      '-
    :expr-multiplication   '*
    :expr-division         '/
    :expr-not              'not
    :distribution-event-gt '>
    :conditioned-by-expr   'iql/condition))

(defmulti node->sexpr
  "Converts a parse tree node into an s-expression."
  tree/tag
  :hierarchy #'hierarchy)

(defmethod node->sexpr :default
  [node]
  (node->sexpr (tree/only-child-node node)))

(defmethod node->sexpr :expr-prefix
  [node]
  ;; FIXME
  `(~'not ~(node->sexpr (tree/only-child-node node))))

(defmethod node->sexpr :expr-infix
  [node]
  (let [child-nodes (tree/child-nodes node)
        operator (infix-operator node)]
    `(~operator ~@(map node->sexpr child-nodes))))

(defmethod node->sexpr :expr-suffix
  [node]
  (let [sym (case (tree/tag node)
              :expr-null 'nil?
              :expr-not-null 'some?)]
    `(~sym ~(node->sexpr (tree/only-child-node node)))))

(defmethod node->sexpr :event-infix
  [node]
  (let [child-nodes (tree/child-nodes node)
        operator (infix-operator node)]
    `(list (quote ~operator) ~@(map node->sexpr child-nodes))))

(defmethod node->sexpr :simple-symbol
  [node]
  (eval-literal node))

(defmethod node->sexpr :qualified-symbol
  [node]
  (apply symbol (map (comp name eval-literal) (tree/child-nodes node))))

(defmethod node->sexpr :value
  [node]
  (eval-literal (tree/only-child-node node)))

(defn condition
  [model conditions]
  (cond-> model
    (every? some? (vals conditions))
    (gpm/condition conditions)))

(def namespaces
  {'inferenceql.inference.gpm {}
   'clojure.core {'not not
                  '> >
                  '>= >=
                  '= =
                  '<= <=
                  '< <
                  '+ +
                  '- -
                  '* *
                  '/ /}
   'iql {'prob (comp math/exp gpm/logprob)
         'pdf (comp math/exp gpm/logpdf)
         'condition condition}})

(defn sexpr->fn
  [sexpr env]
  (fn [tuple]
    (let [bindings (merge (zipmap (tuple/attributes tuple)
                                  (repeat nil))
                          (tuple/->map tuple)
                          env)
          ;; FIXME write a function to produce this automatically
          ;; from `env`
          opts {:namespaces namespaces
                :bindings bindings}]
      (try (sci/eval-string (pr-str sexpr) opts)
           (catch clojure.lang.ExceptionInfo ex
             (def tuple tuple)
             (if-let [[_ sym] (re-find #"Could not resolve symbol: (.+)$"
                                       (ex-message ex))]
               (let [sym (symbol sym)]
                 (if (contains? (set (tuple/attributes tuple))
                                sym)
                   nil
                   (throw (ex-info (str "Could not resolve symbol: " (pr-str sym))
                                   {:symbol sym
                                    :env bindings}))))
               (throw ex)))))))

(defmethod node->sexpr :variable-expr
  [node]
  (keyword (eval-literal (tree/only-child-node node))))

;;; operation

(defn lookup-op
  [relation-name]
  {:operation/type :operation.type/lookup
   :operation/relation-name relation-name})

(defn select-op
  [op attrs]
  {:operation/type :operation.type/select
   :operation/sexpr attrs
   :operation/operation op})

(defn extended-project-op
  [op coll]
  ;; `coll` is a sequence of (s-expression, attribute) pairs.
  (let [terms (mapv #(zipmap [:term/sexpr :term/attribute] %)
                    coll)]
    {:operation/type :operation.type/extended-project
     :operation/terms terms
     :operation/operation op}))

(defn generate-op
  [op variables]
  {:operation/type :operation.type/generate
   :operation/operation op
   :operation/variables variables})

(defn limit-op
  [op limit]
  {:operation/type :operation.type/limit
   :operation/limit limit
   :operation/operation op})

;;; plan

(defmulti plan
  "Returns a query plan for the provided parse tree node. The 2-arity version is
  used when the operation that provides the input relation comes from elsewhere
  in the parse tree."
  (fn
    ([node] (tree/tag node))
    ([node _op] (tree/tag node))))

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
    (lookup-op sym)))

(defmethod plan :generate-expr
  [node]
  (let [sym (or (eval-literal-in node [:model-expr 0])
                'model)
        op (model/lookup-op sym)
        variables (let [generate-list-nodes (tree/child-nodes (tree/get-node-in node [:generate-clause :generate-list]))]
                    (if (star? (first generate-list-nodes))
                      '*
                      (map eval-literal generate-list-nodes)))]
    (generate-op op variables)))

(defmethod plan :where-clause
  [node op]
  (let [sexpr (-> (tree/get-node node :scalar-expr)
                  (node->sexpr))]
    (select-op op sexpr)))

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
  (let [selection->pair (fn [node]
                          (assert (= :selection (tree/tag node)))
                          (if-let [expr (tree/get-node node :scalar-expr)]
                            (let [sexpr (node->sexpr expr)
                                  output-attribute (output-attribute node)]
                              [sexpr output-attribute])
                            (recur (tree/only-child-node node))))]
    (if (star? node)
      op
      (let [projections (map selection->pair (tree/child-nodes (tree/get-node node :select-list)))]
        (extended-project-op op projections)))))

(defmethod plan :limit-clause
  [node op]
  (let [limit (eval-literal-in node [:int])]
    (limit-op op limit)))

(defmethod plan :select-expr
  [node]
  (->> (plan (tree/get-node node :from-clause))
       (plan (tree/get-node node :where-clause))
       (plan (tree/get-node node :select-clause))
       (plan (tree/get-node node :limit-clause))))

;;; eval

(defmulti eval (fn [op _env] (:operation/type op)))

(defmethod eval :operation.type/lookup
  [op env]
  (let [{:operation/keys [relation-name]} op]
    (env/get env relation-name)))

(defmethod eval :operation.type/project
  [op env]
  (let [{:operation/keys [attributes operation]} op
        rel (eval operation env)]
    (relation/project rel attributes)))

(defmethod eval :operation.type/select
  [op env]
  (let [{:operation/keys [sexpr operation]} op
        rel (eval operation env)
        pred (sexpr->fn sexpr env)]
    (relation/select rel pred)))

(defmethod eval :operation.type/extended-project
  [op env]
  (let [{:operation/keys [terms operation]} op
        coll (map (juxt (comp #(sexpr->fn % env) :term/sexpr) :term/attribute) terms)
        rel (eval operation env)]
    (relation/extended-project rel coll)))

(defmethod eval :operation.type/limit
  [op env]
  (let [{:operation/keys [limit operation]} op
        rel (eval operation env)]
    (relation/limit rel limit)))

(defmethod eval :operation.type/generate
  [op env]
  (let [{:operation/keys [operation variables]} op
        model (model/eval operation env)
        variables (if (= '* variables)
                    (gpm/variables model)
                    variables)
        samples (repeatedly #(gpm/simulate model variables {}))]
    (relation/relation samples variables)))

(defmethod node->sexpr :probability-expr
  [node]
  (let [target (-> node (tree/get-node :distribution-event) (node->sexpr))
        model (or (some-> node (tree/get-node :model-expr) (node->sexpr))
                  'model)]
    `(~'iql/prob ~model ~target)))

(defmethod node->sexpr :density-expr
  [node]
  (let [target (-> node (tree/get-node :density-event) (node->sexpr))
        model (or (some-> node (tree/get-node :model-expr) (node->sexpr))
                  'model)]
    `(~'iql/pdf ~model ~target {})))

(defmethod node->sexpr :density-event-and
  [node]
  (into {}
        (map node->sexpr)
        (tree/child-nodes node)))

(defmethod node->sexpr :density-event-eq
  [node]
  (let [k (node->sexpr (tree/get-node node :variable-expr))
        v (node->sexpr (tree/get-node node :scalar-expr))]
    {k v}))

(comment

 (require '[inferenceql.query.parser :as parser] :reload)

 (node->sexpr
  (parser/parse "model CONDITIONED BY (VAR x=0 AND VAR y=1 AND VAR z=2 * a)" :start :model-expr)
  )

 (node->sexpr
  (parser/parse "7 * PROBABILITY DENSITY OF VAR x = 3 UNDER model CONDITIONED BY (VAR x=0 AND VAR y=1 AND VAR z=2 * a)" :start :scalar-expr)
  )

 (plan (parser/parse "SELECT (PROBABILITY DENSITY OF VAR x = x UNDER model) FROM data"))

 (node->sexpr (parser/parse "1 + (PROBABILITY OF VAR x > 3 + y UNDER model)" :start :scalar-expr))
 (node->sexpr (parser/parse "PROBABILITY OF VAR x > 3 + y UNDER model" :start :scalar-expr))

 (node->sexpr (parser/parse "x is not null" :start :scalar-expr))

 (parser/parse "PROBABILITY OF VAR x > 3 * 4 + 7 UNDER model" :start :probability-expr)
 (parser/parse "PROBABILITY OF 3 * 4 + 7 > VAR x UNDER model" :start :probability-expr)
 (parser/parse "PROBABILITY OF VAR x > VAR y UNDER model" :start :probability-expr)
 (parser/parse "PROBABILITY OF VAR x > 7 UNDER model" :start :probability-expr)
 (parser/parse "PROBABILITY OF VAR x = 0 AND VAR y = 0 OR VAR z = 0 UNDER model" :start :probability-expr)
 (parser/parse "PROBABILITY OF VAR x = 0 OR VAR y = 0 AND VAR z = 0 UNDER model" :start :probability-expr)
 (plan (parser/parse "SELECT * FROM data WHERE x IS NOT NULL"))

 (plan (parser/parse "select * from data limit 10"))

 )
