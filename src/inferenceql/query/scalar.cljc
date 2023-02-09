(ns inferenceql.query.scalar
  (:refer-clojure :exclude [eval])
  (:require [clojure.core.match :as match]
            [clojure.edn :as edn]
            [clojure.math :as math]
            [clojure.walk :as walk]
            [cognitect.anomalies :as-alias anomalies]
            [inferenceql.inference.approximate :as approx]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.inference.search.crosscat :as search]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.tuple :as tuple]
            [inferenceql.query.io :as io]
            [sci.core :as sci]))



(defn relation-plan
  [node]
  (let [plan (clojure.core/requiring-resolve 'inferenceql.query.plan/plan)]
    (walk/postwalk-replace {'data ''data} (plan node))))

(defn plan
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

    [:expr-binop left [:binop [:is _]]       right] `(~'=         ~(plan left) ~(plan right))
    [:expr-binop left [:binop [:is-not & _]] right] `(~'not=      ~(plan left) ~(plan right))
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

    ;; how do I get the selection here right
    ;[:search-expr  _relevance _prob _to relation _under model _in _context _of s] `(~'iql/row-search  ~'row ~(plan model) (~'iql/relation-eval ~(relation-plan relation) {~(quote 'data) ~'data} {}) ~(plan s))
    [:search-expr  _relevance _prob _to _rowids ids _under model _in _context _of s] `(~'iql/row-search  ~'row ~(plan model) ~(plan ids) ~s)

    [:mutual-info-expr           _m _i _of lhs _with rhs _under model] `(~'iql/mutual-info        ~(plan model) ~(vec (plan lhs)) ~(vec (plan rhs)))
    [:approx-mutual-info-expr _a _m _i _of lhs _with rhs _under model] `(~'iql/approx-mutual-info ~(plan model) ~(vec (plan lhs)) ~(vec (plan rhs)))

    [:model-expr child] (plan child)
    [:model-expr "(" child ")"] (plan child)
    [:conditioned-by-expr model _conditioned _by event] `(~'iql/condition ~(plan model) ~(plan event))
    [:constrained-by-expr model _constrained _by event] `(~'iql/constrain ~(plan model) ~(plan event))

    [:incorporate-expr _incorporate  label _into model] `(~'iql/incorporate ~(plan model) ~(vec label))
    [:incorporate-expr "(" child ")"] (plan child)
    [:value [:null _]] nil
    [:value [_ child]] (edn/read-string child)

    [:variable _var child] (keyword (plan child))
    [:variable-list & variables] (map plan variables)

    [:int-list & ids] `(~'iql/get-ids ~ids)

    [:simple-symbol s] (symbol s)))

(defn inference-event
  [event]
  (walk/postwalk (fn [x]
                   (cond (vector? x) (seq x)
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
  (let [conditions (update-keys conditions keyword)]
    (cond-> model
      (every? some? (vals conditions))
      (gpm/condition conditions))))

(defn constrain
  [model event]
  (let [event (inference-event event)]
    (cond-> model
      (empty? (filter nil? (tree-seq seqable? seq event)))
      (gpm/constrain event
                     {:operation? seq?
                      :operands rest
                      :operator first
                      :variable? symbol?}))))

(defn mutual-info
  [model event-a event-b]
  (let [event-a (inference-event event-a)
        event-b (inference-event event-b)]
    (gpm/mutual-info model event-a event-b)))

(defn approx-mutual-info
  [model vars-lhs vars-rhs]
  (approx/mutual-info model vars-lhs vars-rhs {} 1000))


(defn convert [v]
  (if (contains? #{"true" "false"} v)
    (boolean v)
    (Integer. v)))

(defn row-label [item]
  (when (vector? item)
    (when (contains? #{:int :bool} (first item))
      (convert (second item)))))

(defn incorporate
  [model label-list]
  (let [col (->> label-list
                 (remove #(contains? #{:label-list "(" ")" "," } %))
                 (map #(map row-label %))
                 (map #(vec (remove nil? %)))
                 (vec)
                 (into {}))]
  (search/incorporate-labels model col)))

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

(defn relation-eval
  [plan env bindings]
  (let [eval (clojure.core/requiring-resolve 'inferenceql.query.plan/eval)]
    (eval plan env bindings)))

(defn get-ids [l]
  (map second (remove #(= "," %) l)))

;; XXX: next step, slurp it. next step: make it not insane.


#_(def the-data [
             {:ROWID "0" :x 0 :y 3 :a 6}
             {:ROWID "1" :x 1 :y 4 :a 7}
             {:ROWID "2" :x 2 :y 5 :a 8}
             ])

(def the-data (io/slurp-csv "temp-data.csv"))

(defn row-search
  [row model ids col-sym-expr]
  (if (.contains ids (str (get row 'ROWID)))
    1.0
    (let [col (keyword (second col-sym-expr))
          row (update-keys row keyword)
          comparison-symbols (filter #(.contains ids (str (get % 'ROWID))) the-data)
          comparison (map #(update-keys % keyword) comparison-symbols)
          ]
      (search/relevance-probability model
                             row
                             comparison
                             col
                             ))))

(def namespaces
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
                  '/ (nil-safe (auto-unbox /))}
   'iql {'prob prob
         'pdf pdf
         'condition condition
         'constrain constrain
         'mutual-info mutual-info
         'approx-mutual-info approx-mutual-info
         'incorporate incorporate
         'relation-eval relation-eval
         'row-search row-search
         'get-ids get-ids
         }})

(defn eval
  [sexpr env bindings & tuples]
  (let [env (merge env bindings {'row (first tuples)})
        tuple-map (fn [tuple]
                    (merge (zipmap (tuple/attributes tuple)
                                   (repeat nil))
                           (when-let [tuple-name (tuple/name tuple)]
                             (zipmap (map #(symbol (str tuple-name "." %))
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
        opts {:namespaces namespaces
              :bindings bindings}]
    (try (sci/eval-string (pr-str sexpr) opts)
         (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) ex
           (if-let [[_ sym] (re-find #"Could not resolve symbol: (.+)$"
                                     (ex-message ex))]
             (let [sym (symbol sym)]
               (when-not (contains? attributes sym)
                 (throw (ex-info (str "Could not resolve symbol: " (pr-str sym))
                                 {::anomalies/category ::anomalies/incorrect
                                  symbol sym
                                  :env bindings}))))
             (throw ex))))))
