(ns gensql.query.scalar
  (:refer-clojure :exclude [eval])
  (:require [clojure.core.match :as match]
            [clojure.edn :as edn]
            [clojure.math :as math]
            [clojure.walk :as walk]
            [cognitect.anomalies :as-alias anomalies]
            [gensql.inference.approximate :as approx]
            [gensql.inference.gpm :as gpm]
            ;; [gensql.inference.search.crosscat :as crosscat]
            [gensql.query.cache :as cache]
            #?(:clj [gensql.query.generative-table :as generative-table])
            [gensql.query.literal :as literal]
            [gensql.query.parser.tree :as tree]
            [gensql.query.relation :as relation]
            [gensql.query.tuple :as tuple]
            [medley.core :as medley]
            [sci.core :as sci]))


(defn id-node->str
  "Returns the string representation of an identifier node."
  [node]
  (-> node tree/only-child-node (nth 1)))

(declare plan)

(defn ^:private conditioned-by-plan*
  "`plan` helper that generates plans for CONDITIONED BY nodes.

  NB: Exists because https://clojure.atlassian.net/browse/CLJ-1852 prevents us
  from directly adding these rules into `plan`."
  [node]
  (match/match node
    [:conditioned-by-expr model _conditioned _by [:star _]]
    `(~'iql/condition-all ~(plan model))
    [:conditioned-by-expr model _conditioned _by [:star _] [:conditioned-by-except-clause & except-children]]
    `(~'iql/condition-all-except ~(plan model) ~(plan (into [:conditioned-by-except-clause] except-children)))
    [:conditioned-by-expr model _conditioned _by child]
    `(~'iql/condition ~(plan model) ~(plan child))
    [:conditioned-by-except-clause _except model-var-list]
    (plan model-var-list)))

(defn plan
  "Given a parse tree/node, returns an execution plan."
  [node]
  (let [ws-free-node (into (empty node)
                           (remove tree/whitespace?)
                           node)]
    (match/match ws-free-node
      [:scalar-expr child] (plan child)
      [:scalar-expr-group "(" child ")"] (plan child)

      [:expr-not _not child] `(~'not ~(plan child))

      [:expr-disjunction left _ right] `(~'or ~(plan left) ~(plan right))
      [:expr-conjunction left _ right] `(~'and ~(plan left) ~(plan right))
      [:expr-addition left _ right] `(~'+ ~(plan left) ~(plan right))
      [:expr-addition left _ right] `(~'+ ~(plan left) ~(plan right))
      [:expr-subtraction left _ right] `(~'- ~(plan left) ~(plan right))
      [:expr-multiplication left _ right] `(~'* ~(plan left) ~(plan right))
      [:expr-division left _ right] `(~'/ ~(plan left) ~(plan right))

      [:expr-function-call-log _log child _] `(~'log ~(plan child))

      [:expr-binop left [:binop [:is _]] right] `(~'= ~(plan left) ~(plan right))
      [:expr-binop left [:binop [:is-not & _]] right] `(~'not= ~(plan left) ~(plan right))
      ;; MUST not str-ify below, binops aren't identifiers.
      [:expr-binop left [:binop s] right] `(~(symbol s) ~(plan left) ~(plan right))

      [:distribution-event child] (plan child)

      [:distribution-event-or left _or right] [:or (plan left) (plan right)]
      [:distribution-event-and left _and right] [:and (plan left) (plan right)]

      [:distribution-event-binop (variable :guard (tree/tag-pred :variable)) [:binop s] (scalar :guard (tree/tag-pred :scalar-expr))] [(keyword s) (plan variable) (plan scalar)]
      [:distribution-event-binop (scalar :guard (tree/tag-pred :scalar-expr)) [:binop s] (variable :guard (tree/tag-pred :variable))] [(keyword s) (plan variable) (plan scalar)]

      [:distribution-event-group "(" child ")"] (plan child)

      [:density-event child] (plan child)
      [:density-event-and & children] (into {} (comp (filter tree/branch?) (map plan)) children)

      [:density-event-eq (variable :guard (tree/tag-pred :variable)) _= (scalar :guard (tree/tag-pred :scalar-expr))] {(plan variable) (plan scalar)}
      [:density-event-eq (scalar :guard (tree/tag-pred :scalar-expr)) _= (variable :guard (tree/tag-pred :variable))] {(plan variable) (plan scalar)}

      [:density-event-group "(" child ")"] (plan child)

      [:probability-expr _prob _of event _under model] `(~'iql/prob ~(plan model) ~(plan event))
      [:density-expr _prob _density _of event _under model] `(~'iql/pdf ~(plan model) ~(plan event))

      [:mutual-info-expr _m _i _of lhs _with rhs _under model] `(~'iql/mutual-info ~(plan model) ~(vec (plan lhs)) ~(vec (plan rhs)))
      [:approx-mutual-info-expr _a _m _i _of lhs _with rhs _under model] `(~'iql/approx-mutual-info ~(plan model) ~(vec (plan lhs)) ~(vec (plan rhs)))

      [:model-expr child] (plan child)
      [:model-expr "(" child ")"] (plan child)

      #?@(:clj [[:generative-table-expr _generative _table relation]
                (let [query-plan (requiring-resolve 'gensql.query.plan/plan)]
                  `(~'iql/eval-relation-plan (~'quote ~(query-plan relation))))])

      ;; Matches either :conditioned-by-expr or :conditioned-by-except-clause
      ;; and defers to conditioned-by-plan* to avoid https://clojure.atlassian.net/browse/CLJ-1852
      [(:or :conditioned-by-expr :conditioned-by-except-clause) & _] (conditioned-by-plan* ws-free-node)

      [:constrained-by-expr model _constrained _by event] `(~'iql/constrain ~(plan model) ~(plan event))


      [:value child] (literal/read child)

      [:variable _var child] (id-node->str child)
      [:variable-list & variables] (into [] (comp (filter tree/branch?) (map plan)) variables) ; remove commas

      [:identifier child] (plan child)
      [:delimited-symbol s] (list 'iql/safe-get 'iql-bindings s)
      [:simple-symbol s] (list 'iql/safe-get 'iql-bindings s))))

(defn inference-event
  [event]
  (walk/postwalk (fn [x]
                   (cond (vector? x) (seq x)
                         (keyword? x) (symbol x)
                         :else x))
                 event))

(defn safe-get
  "Looks up an identifier id in map m. If not found, throws an exception."
  [m id]
  (let [result (get m id ::not-found)]
    (if (= result ::not-found)
      (throw (ex-info (str "Could not resolve identifier: " (pr-str id))
                      {::anomalies/category ::anomalies/incorrect
                       :identifier id
                       :env m}))
      result)))

(def prob
  (cache/lru
    (fn prob*
      [model event]
      (let [event (inference-event event)]
        (math/exp (gpm/logprob model event))))))

(def pdf
  (cache/lru
    (fn pdf*
      [model event]
      (let [event (update-keys event str)]
        (math/exp (gpm/logpdf model event {}))))))

(def condition
  (cache/lru
    (fn condition*
      [model conditions]
      (let [conditions (-> (medley/filter-vals some? conditions)
                           (update-keys str))]
        (cond-> model
                (seq conditions)
                (gpm/condition conditions))))))

(defn condition-all
  "Retrieves all variables from the model and conditions them based on the
  value found in the bindings, which includes the current tuple/row.

  The 3-arity version takes an additional coll of vars to exclude."
  ([model bindings]
   (condition-all model #{} bindings))
  ([model exclusions bindings]
   (let [exclusions (set exclusions)
         condition-vars (into []
                              (comp
                                (map name)
                                (filter (complement exclusions)))
                              (gpm/variables model))
         conditions (reduce (fn [conditions variable]
                              (cond-> conditions
                                      (contains? bindings variable)
                                      (assoc variable (get bindings variable))))
                            {}
                            condition-vars)]
     (condition model conditions))))

(defn operation?
  "Given an event form, returns `true` if the form is an operation."
  [x]
  (seq? x))

(defn operands
  "Given an operation, returns the operands for that operation."
  [operation]
  (rest operation))

(defn operator
  "Given an operation, returns the operator for that operation."
  [operation]
  (first operation))

(defn variable?
  "Returns `true` if `x` is a form that represents a variable."
  [x]
  (symbol? x))

(defn strip-nils
  [event]
  (walk/postwalk (fn [form]
                   (if-not (operation? form)
                     form
                     (let [nil-count (count (filter nil? form))
                           value-count (count (remove (some-fn variable? nil?) form))]
                       (cond (zero? nil-count) form
                             (zero? value-count) nil
                             :else (remove nil? form)))))
                 event))

(def constrain
  (cache/lru
    (fn constrain*
      [model event]
      (let [event (-> event
                      (strip-nils)
                      (inference-event))]
        (cond-> model
                (some? event)
                (gpm/constrain event
                               {:operation? operation?
                                :operands operands
                                :operator operator
                                :variable? variable?}))))))

(def mutual-info
  (cache/lru
    (fn mutual-info*
      [model event-a event-b]
      (let [event-a (inference-event event-a)
            event-b (inference-event event-b)]
        (gpm/mutual-info model event-a event-b)))))

(defn approx-mutual-info
  [model vars-lhs vars-rhs]
  (approx/mutual-info model vars-lhs vars-rhs {} 1000))

#_
(defn incorporate
  [model rel]
  (let [new-columns (fn [model rel]
                      (set/difference (set (relation/attributes rel))
                                      (set (gpm/variables model))))
        incorporate-column (fn [model rel col]
                             (let [column-map (into {}
                                                    (map-indexed (fn [i tup]
                                                                   (when-some [v (tuple/get tup col)]
                                                                     [i v])))
                                                    rel)]))]
    (if-let [new-columns (seq (new-columns model rel))]
      (reduce #(crosscat/incorporate-labels )
              model
              new-columns)
      (reduce gpm/incorporate
              model
              (relation/tuples rel)))))

(defn unbox
  "Returns the first value of relation `rel`."
  [rel]
  (->> rel
       (relation/tuples)
       (first)
       (tuple/->vector)
       (first)))

(defn auto-unbox
  "Wraps `f` with a function that calls `unbox` on any arguments that are
  relations."
  [f]
  (fn [& args]
    (->> args
         (map #(cond-> % (relation/relation? %) (unbox)))
         (apply f))))

(defn nil-safe
  "Wraps `f` with a function that reutnrs `nil` if any of its arguments are
  `nil`."
  [f]
  (fn [& args]
    (when-not (some nil? args)
      (apply f args))))

(defn namespaces
  #?(:clj [env bindings]
     :cljs [])
  {'gensql.inference.gpm {}
   'clojure.core {'not not
                  '> (nil-safe (auto-unbox >))
                  '>= (nil-safe (auto-unbox >=))
                  '= (auto-unbox =)
                  '<= (nil-safe (auto-unbox <=))
                  '< (nil-safe (auto-unbox <))
                  '+ (nil-safe (auto-unbox +))
                  '- (nil-safe (auto-unbox -))
                  '* (nil-safe (auto-unbox *))
                  '/ (nil-safe (auto-unbox /))
                  'log (nil-safe (auto-unbox math/log))}
   'iql {'safe-get safe-get
         'prob prob
         'pdf pdf
         #?@(:clj ['eval-relation-plan
                   (let [eval (requiring-resolve 'gensql.query.plan/eval)]
                     #(generative-table/generative-table (eval % env bindings)))])
         'condition-all #(condition-all % bindings)
         'condition-all-except #(condition-all %1 %2 bindings)
         'condition condition
         'constrain constrain
         'mutual-info mutual-info
         'approx-mutual-info approx-mutual-info
         ;; 'incorporate incorporate
         }})

(defn eval
  "Evaluates a scalar-based sexpr given the environment, bindings, and
  (optional) tuples/rows.

  Params:
  - sexpr: The scalar-based S-expression to evaluate in SCI.
  - env: Holds the models and relations to use in the evaluation.
  - bindings: Extra bindings, like WITH-based CTEs.
  - tuples: The tuples/rows to use in the evaluation."
  ;; TODO: Use a SCI ctx for environment shared across tuples?
  ;; NB: never actually passes in more than one tuple
  [sexpr env bindings & tuples]
  (tap> #:scalar.eval{:in-env env :in-bindings bindings
                      :sexpr  (pr-str sexpr) :tuple-sample (take 3 tuples)})
  (let [env' (merge env bindings)
        tuple-map (fn tuple-map [tuple]
                    (merge (zipmap (tuple/attributes tuple)
                                   (repeat nil))
                           (when-let [tuple-name (tuple/name tuple)]
                             (zipmap (map #(str tuple-name "." %)
                                          (tuple/attributes tuple))
                                     (repeat nil)))
                           env'
                           (tuple/->map tuple)))
        attributes (into #{} (map tuple/attributes) tuples)
        sci-bindings (into env'
                           (map tuple-map)
                           tuples)
        ;; FIXME write a function to produce this automatically
        ;; from `env`
        opts {:namespaces #?(:clj (namespaces env' sci-bindings)
                             :cljs (namespaces))
              :bindings (merge sci-bindings
                               {'iql-bindings sci-bindings})}]
    ;; ensure pr-str doesn't truncate by setting print-length to nil
    (binding [*print-length* nil]
      (try (let [sci-result (sci/eval-string (pr-str sexpr) opts)]
             (tap> #:scalar.eval{:opts opts
                                 :result sci-result})
             sci-result)

           (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) ex
             (if-let [[_ id] (re-find #"Could not resolve identifier: (.+)$"
                                      (ex-message ex))]
               (when-not (contains? attributes id)
                 (throw (ex-info (str "Could not resolve identifier: " (pr-str id))
                                 {::anomalies/category ::anomalies/incorrect
                                  :identifier (pr-str id)
                                  :sexpr (pr-str sexpr)
                                  :env sci-bindings})))
               (throw ex)))))))
