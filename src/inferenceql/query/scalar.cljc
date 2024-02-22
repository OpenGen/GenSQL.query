(ns inferenceql.query.scalar
  (:refer-clojure :exclude [eval])
  (:require [clojure.core.match :as match]
            [clojure.edn :as edn]
            [clojure.math :as math]
            [clojure.walk :as walk]
            [cognitect.anomalies :as-alias anomalies]
            [inferenceql.inference.approximate :as approx]
            [inferenceql.inference.gpm :as gpm]
            ;; [inferenceql.inference.search.crosscat :as crosscat]
            #?(:clj [inferenceql.query.generative-table :as generative-table])
            [inferenceql.query.literal :as literal]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.string :as query.string]
            [inferenceql.query.tuple :as tuple]
            [medley.core :as medley]
            [sci.core :as sci]))

(defn plan
  "Given a parse tree/node, returns an execution plan."
  [node]
  (match/match (into (empty node)
                     (remove tree/whitespace?)
                     node)
    [:scalar-expr child]               (plan child)
    [:scalar-expr-group "(" child ")"] (plan child)

    [:expr-not _not child] `(~'not ~(plan child))

    [:expr-disjunction    left _ right] `(~'or  ~(plan left) ~(plan right))
    [:expr-conjunction    left _ right] `(~'and ~(plan left) ~(plan right))
    [:expr-addition       left _ right] `(~'+   ~(plan left) ~(plan right))
    [:expr-addition       left _ right] `(~'+   ~(plan left) ~(plan right))
    [:expr-subtraction    left _ right] `(~'-   ~(plan left) ~(plan right))
    [:expr-multiplication left _ right] `(~'*   ~(plan left) ~(plan right))
    [:expr-division       left _ right] `(~'/   ~(plan left) ~(plan right))

    [:expr-function-call-log _log child _] `(~'log ~(plan child))

    [:expr-binop left [:binop [:is _]]       right] `(~'=         ~(plan left) ~(plan right))
    [:expr-binop left [:binop [:is-not & _]] right] `(~'not=      ~(plan left) ~(plan right))
    ;; MUST not munge below, binops aren't identifiers.
    [:expr-binop left [:binop s]             right] `(~(symbol s) ~(plan left) ~(plan right))

    [:distribution-event child] (plan child)

    [:distribution-event-or  left _or  right] [:or  (plan left) (plan right)]
    [:distribution-event-and left _and right] [:and (plan left) (plan right)]

    [:distribution-event-binop (variable :guard (tree/tag-pred :variable))    [:binop s] (scalar   :guard (tree/tag-pred :scalar-expr))] [(keyword s) (plan variable) (plan scalar)]
    [:distribution-event-binop (scalar   :guard (tree/tag-pred :scalar-expr)) [:binop s] (variable :guard (tree/tag-pred :variable))]    [(keyword s) (plan variable) (plan scalar)]

    [:distribution-event-group "(" child ")"] (plan child)

    [:density-event child] (plan child)
    [:density-event-and & children] (into {} (comp (filter tree/branch?) (map plan)) children)

    [:density-event-eq (variable :guard (tree/tag-pred :variable))    _= (scalar   :guard (tree/tag-pred :scalar-expr))] {(plan variable) (plan scalar)}
    [:density-event-eq (scalar   :guard (tree/tag-pred :scalar-expr)) _= (variable :guard (tree/tag-pred :variable))]    {(plan variable) (plan scalar)}

    [:density-event-group "(" child ")"] (plan child)

    [:probability-expr _prob          _of event _under model] `(~'iql/prob ~(plan model) ~(plan event))
    [:density-expr     _prob _density _of event _under model] `(~'iql/pdf  ~(plan model) ~(plan event))

    [:mutual-info-expr           _m _i _of lhs _with rhs _under model] `(~'iql/mutual-info        ~(plan model) ~(vec (plan lhs)) ~(vec (plan rhs)))
    [:approx-mutual-info-expr _a _m _i _of lhs _with rhs _under model] `(~'iql/approx-mutual-info ~(plan model) ~(vec (plan lhs)) ~(vec (plan rhs)))

    [:model-expr child] (plan child)
    [:model-expr "(" child ")"] (plan child)

    #?@(:clj [[:generative-table-expr _generative _table relation]
              (let [query-plan (requiring-resolve 'inferenceql.query.plan/plan)]
                `(~'iql/eval-relation-plan (~'quote ~(query-plan relation))))])

    [:conditioned-by-expr model _conditioned _by "*"]   `(~'iql/condition-all ~(plan model))
    [:conditioned-by-expr model _conditioned _by event] `(~'iql/condition ~(plan model) ~(plan event))
    [:constrained-by-expr model _constrained _by event] `(~'iql/constrain ~(plan model) ~(plan event))

    [:value child] (literal/read child)

    [:variable _var child] (keyword (plan child))
    [:variable-list & variables] (map plan variables)

    [:simple-symbol s] (query.string/safe-symbol s)))

(defn inference-event
  [event]
  (walk/postwalk (fn [x]
                   (cond (vector? x) (seq x)
                         ;; NB: Do not munge kws here, could potentially have
                         ;; keyword ops like :<, :<=, etc. Identifiers already
                         ;; munged in `plan`.
                         (keyword? x) (symbol x)
                         :else x))
                 event))

(defn prob
  [model event]
  (let [event (inference-event event)]
    (math/exp (gpm/logprob model event))))

(defn pdf
  [model event]
  (let [event (update-keys event keyword)]
    (math/exp (gpm/logpdf model event {}))))

(defn condition
  [model conditions]
  (let [conditions (-> (medley/filter-vals some? conditions)
                       (update-keys keyword))]
    (cond-> model
      (seq conditions)
      (gpm/condition conditions))))

(defn condition-all
  [model bindings]
  (let [conditions (reduce (fn [conditions variable]
                             (cond-> conditions
                               (contains? bindings variable)
                               (assoc variable (get bindings variable))))
                           {}
                           (map query.string/safe-symbol (gpm/variables model)))]
    (condition model conditions)))

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

(defn constrain
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
                      :variable? variable?}))))

(defn mutual-info
  [model event-a event-b]
  (let [event-a (inference-event event-a)
        event-b (inference-event event-b)]
    (gpm/mutual-info model event-a event-b)))

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
  {'inferenceql.inference.gpm {}
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
   'iql {'prob prob
         'pdf pdf
         #?@(:clj ['eval-relation-plan
                   (let [eval (requiring-resolve 'inferenceql.query.plan/eval)]
                     #(generative-table/generative-table (eval % env bindings)))])
         #?@(:clj ['condition-all #(condition-all % bindings)])
         'condition condition
         'constrain constrain
         'mutual-info mutual-info
         'approx-mutual-info approx-mutual-info
         ;; 'incorporate incorporate
         }})

(defn eval
  "Evaluates a scalar-based sexpr given the environment, bindings, and
  (optional) tuples/rows."
  [sexpr env bindings & tuples]
  (let [env (merge env bindings)
        tuple-map (fn [tuple]
                    (merge (zipmap (tuple/attributes tuple)
                                   (repeat nil))
                           (when-let [tuple-name (tuple/name tuple)]
                             (zipmap (map #(query.string/safe-symbol (str tuple-name "." %))
                                          (tuple/attributes tuple))
                                     (repeat nil)))
                           env
                           (tuple/->map tuple)))
        attributes (into #{} (map tuple/attributes) tuples)
        bindings (into (merge env bindings)
                       (map tuple-map)
                       tuples)
        ;; FIXME write a function to produce this automatically
        ;; from `env`
        opts {:namespaces #?(:clj (namespaces env bindings)
                             :cljs (namespaces))
              :bindings bindings}]
    (try (sci/eval-string (pr-str sexpr) opts)
         (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) ex
           (if-let [[_ sym] (re-find #"Could not resolve symbol: (.+)$"
                                     (ex-message ex))]
             (let [sym (query.string/safe-symbol sym)]
               (when-not (contains? attributes sym)
                 (throw (ex-info (str "Could not resolve symbol: " (pr-str sym))
                                 {::anomalies/category ::anomalies/incorrect
                                  symbol (pr-str sym)
                                  :sexpr (pr-str sexpr)
                                  :env bindings}))))
             (throw ex))))))
