(ns inferenceql.query.plan
  (:refer-clojure :exclude [eval])
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [hashp.core]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.environment :as env]
            [inferenceql.query.model :as model]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.tuple :as tuple]
            [instaparse.core :as instaparse]
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

;;; eval-literal

(defmulti eval-literal tree/tag)

(defmethod eval-literal :bool
  [node]
  (edn/read-string (tree/only-child node)))

(defmethod eval-literal :float
  [node]
  (edn/read-string (tree/only-child node)))

(defmethod eval-literal :int
  [node]
  (edn/read-string (tree/only-child node)))

(defmethod eval-literal :nat
  [node]
  (edn/read-string (tree/only-child node)))

(defmethod eval-literal :simple-symbol
  [node]
  (symbol (tree/only-child node)))

(defmethod eval-literal :string
  [node]
  (edn/read-string (tree/only-child node)))

(defmethod eval-literal nil
  [_]
  nil)

(def eval-literal-in (comp eval-literal tree/get-node-in))

;;; expr->sexpr

(def hierarchy
  (-> (make-hierarchy)
      (derive :expr-disjunction    :expr-infix)
      (derive :expr-conjunction    :expr-infix)
      (derive :expr-gt             :expr-infix)
      (derive :expr-geq            :expr-infix)
      (derive :expr-eq             :expr-infix)
      (derive :expr-leq            :expr-infix)
      (derive :expr-lt             :expr-infix)
      (derive :expr-addition       :expr-infix)
      (derive :expr-subtraction    :expr-infix)
      (derive :expr-multiplication :expr-infix)
      (derive :expr-division       :expr-infix)
      (derive :expr-not            :expr-prefix)))

(defn infix-sexpr-operator
  [node]
  (case (tree/tag node)
    :expr-disjunction    'or
    :expr-conjunction    'and
    :expr-gt             '>
    :expr-geq            '>=
    :expr-eq             '=
    :expr-leq            '<=
    :expr-lt             '<
    :expr-addition       '+
    :expr-subtraction    '-
    :expr-multiplication '*
    :expr-division       '/
    :expr-not            'not))

(defmulti scalar-expr->sexpr
  "Converts an `:expr` parse tree node into an s-expression."
  tree/tag
  :hierarchy #'hierarchy)

(defmethod scalar-expr->sexpr :scalar-expr
  [node]
  (scalar-expr->sexpr (tree/only-child node)))

(defmethod scalar-expr->sexpr :scalar-expr-group
  [node]
  (scalar-expr->sexpr (tree/only-child-node node)))

(defmethod scalar-expr->sexpr :expr-prefix
  [node]
  `(~'not ~(scalar-expr->sexpr (tree/only-child-node node))))

(defmethod scalar-expr->sexpr :expr-infix
  [node]
  (let [cs (tree/child-nodes node)
        operator (infix-sexpr-operator node)]
    `(~operator ~@(map scalar-expr->sexpr cs))))

(defmethod scalar-expr->sexpr :attribute-name
  [node]
  (eval-literal-in node [:simple-symbol]))

(defmethod scalar-expr->sexpr :value
  [node]
  (eval-literal (tree/only-child-node node)))

(defn sexpr->pred
  [sexpr env]
  (fn [tuple]
    (let [m (merge (env/->map env) (tuple/->map tuple))]
      (try (sci/eval-string (pr-str sexpr) {:bindings m})
           (catch clojure.lang.ExceptionInfo ex
             (if-let [[_ sym] (re-find #"Could not resolve symbol: (.*)$"
                                       (ex-message ex))]
               (throw (ex-info (str "Column does not exist: " sym)
                               {:column sym
                                :tuple tuple}))
               (throw ex)))))))

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

;;; plan

(defmulti plan
  "Returns a query plan for the provided parse tree node. The 2-arity version is
  used when the operation that provides the input relation comes from elsewhere
  in the parse tree."
  (fn
    ([node] (tree/tag node))
    ([node _op] (tree/tag node))))

(defmethod plan nil
  [_ op]
  op)

(defmethod plan :from-clause
  [node]
  (if-not node
    (lookup-op 'data)
    (loop [node (tree/only-child-node node)]
      (case (tree/tag node)
        :relation-expr (recur (tree/only-child-node node))
        :simple-symbol (let [table-sym (eval-literal node)]
                         (lookup-op table-sym))
        :select-expr (plan node)
        :generate-expr (let [sym (or (eval-literal-in node [:under-clause 0 0])
                                     'model)
                             op (model/lookup-op sym)
                             variables (let [generate-list-nodes (tree/child-nodes (tree/get-node-in node [:generate-clause :generate-list]))]
                                         (if (star? (first generate-list-nodes))
                                           '*
                                           (map eval-literal generate-list-nodes)))]
                         (generate-op op variables))))))

(defmethod plan :where-clause
  [node op]
  (let [sexpr (-> (tree/get-node node :scalar-expr)
                  (scalar-expr->sexpr))]
    (select-op op sexpr)))

(defn output-attribute
  [node]
  (assert (= :selection (tree/tag node)))
  (if-let [expr (tree/get-node node :scalar-expr)]
    (or (eval-literal-in node [:alias-clause :attribute-name :simple-symbol])
        (-> (parser/unparse expr)
            (string/replace #"\s" "")
            (symbol)))
    (recur (tree/get-node node :selection))))

(defmethod plan :select-clause
  [node op]
  (let [selection->pair (fn [node]
                          (assert (= :selection (tree/tag node)))
                          (if-let [expr (tree/get-node node :scalar-expr)]
                            (let [sexpr (scalar-expr->sexpr expr)
                                  output-attribute (output-attribute node)]
                              [sexpr output-attribute])
                            (recur (tree/only-child node))))]
    (if (star? node)
      op
      (let [projections (map selection->pair (tree/child-nodes (tree/get-node node :select-list)))]
        (extended-project-op op projections)))))

(defmethod plan :select-expr
  [node]
  (->> (plan (tree/get-node node :from-clause))
       (plan (tree/get-node node :where-clause))
       (plan (tree/get-node node :select-clause))))

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
        pred (sexpr->pred sexpr env)]
    (relation/select rel pred)))

(defmethod eval :operation.type/extended-project
  [op env]
  (let [{:operation/keys [terms operation]} op
        coll (map (juxt (comp #(sexpr->pred % env) :term/sexpr) :term/attribute) terms)
        rel (eval operation env)]
    (relation/extended-project rel coll)))

(defmethod eval :operation.type/generate
  [op env]
  (let [{:operation/keys [operation variables]} op
        model (model/eval operation env)
        variables (if (= '* variables)
                    (gpm/variables model)
                    variables)
        samples (repeatedly #(gpm/simulate model variables {}))]
    (relation/relation samples variables)))

(defn q
  [query env]
  (let [node-or-failure (parser/parse query)]
    (if (instaparse/failure? node-or-failure)
      node-or-failure
      (let [plan (plan node-or-failure)]
        (eval plan env)))))

(comment

 (parser/parse "x = 0" :start :expr)

 #_
 (require '[inferenceql.query.parser] :reload)

 (def query "select * from data where x <= 2;")
 (def query "select x, y as z from data where x < 2;")
 (def query "select x from data;")

 (def parse-tree (parser/parse query))
 (def query-plan (plan parse-tree))
 (def env {'data '[{x 0 y 3 z true}
                   {x 1 y 2 z false}
                   {x 2 y 1 z true}
                   {x 3 y 0 z false}]})

 (q query env)
 (q "select * from data where x < 2" env)
 (q "select x from (select x from data) where 1 + x = 3" env)

 ,)
