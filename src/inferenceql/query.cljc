(ns inferenceql.query
  "This file defines functions for parsing, transforming, and executing IQL-SQL
  queries. The public API for this file is the functions are `q`, `pq`, and
  `query-plan`."
  #?(:clj (:require [inferenceql.query.io :as io])
     :cljs (:require-macros [inferenceql.query.io :as io]))
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [datascript.core :as d]
            [instaparse.core :as insta]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.inference.gpm.proto :as gpm.proto]
            [inferenceql.query.math :as math]))

(def entity-var '?entity)
(def default-model-key :model)

(defn safe-get
  [coll k]
  (if (contains? coll k)
    (get coll k)
    (throw (ex-info "Collection does not contain key"
                    {::error ::safe-get
                     ::coll coll
                     ::k k}))))

(defn variable
  "Converts a string, symbol, or keyword to a valid Datalog variable of the same
  name."
  [x]
  ;; Not using a protocol here for now to avoid having to deal with differing
  ;; types in Clojure and ClojureScript.
  (cond (string? x) (symbol (cond->> x
                              (not (string/starts-with? x "?"))
                              (str "?")))
        (symbol? x) (variable (name x))
        (keyword? x) (variable (name x))))

(defn genvar
  "Like `gensym`, but generates Datalog variables."
  ([]
   (variable (gensym)))
  ([prefix-string]
   (variable (gensym prefix-string))))

(defn constrain
  "Constrains the provided generative probabilistic model such that it only
  simulates the provided targets, and is always subject to the provided
  constraints."
  [gpm constrain-targets constrain-constraints]
  (assert vector? constrain-targets)
  (assert map? constrain-constraints)

  (reify gpm.proto/GPM
    (logpdf [this logpdf-targets logpdf-constraints _]
      (let [merged-targets (select-keys logpdf-targets constrain-targets)
            merged-constraints (merge constrain-constraints logpdf-constraints)]
        (gpm/logpdf gpm merged-targets merged-constraints)))

    (simulate [this simulate-targets simulate-constraints n-samples _]
      (let [merged-targets (set/intersection (set constrain-targets) (set simulate-targets))
            merged-constraints (merge constrain-constraints simulate-constraints)]
        (gpm/simulate gpm merged-targets merged-constraints n-samples)))))

(def default-environment
  (merge
   #?(:clj {'clojure.core/merge merge}
      :cljs {'cljs.core/merge merge})
   {'math/exp math/exp
    'clojure.core/merge merge
    'datascript.core/pull datascript.core/pull
    'inferenceql.inference.gpm/logpdf inferenceql.inference.gpm/logpdf

    'clojure.core/=  =
    'clojure.core/>  >
    'clojure.core/>= >=
    'clojure.core/<  <
    'clojrue.core/<= <=}))

(def input-symbols
  (->> default-environment
       (set/map-invert)
       (map (juxt key (comp variable val)))
       (into {})))

;;; Transformation

(defn map-transformer
  "Returns a transformer for use with `insta/transform` that converts one level of
  a Hiccup parse trees into a map. Takes as its argument a sequence of keys for
  the required \"positional\" subtrees for the node.

  For example, for a parse tree for node `A` in the grammar

    A = B C? D?

  would be transformed by `(map-transformer :B)` into a map like

    {:B ... :C ... :D ...}

  where the key `:B` is taken from the argument to `map-transformer`. Keys `:C`
  and `:D` will be omitted in the resulting map if the corresponding nodes were
  not present in the parse tree."
  [& ks]
  (fn [& children]
    (let [[req-children opt-children] (split-at (count ks) children)
          req-map (zipmap ks req-children)
          opt-map (reduce (fn [acc node]
                            (if-let [k (::node-type (meta node))]
                              (assoc acc k node)
                              (let [[k & vs] node
                                    v (or (when (coll? vs)
                                            (if (= 1 (count vs))
                                              (first vs)
                                              (vec vs)))
                                          true)]
                                (assoc acc k v))))
                          {}
                          opt-children)]
      (merge req-map opt-map))))

(defn iobj?
  "Returns `true` if the provided value implements `clojure.lang.IObj`. Only
  values that implement `clojure.lang.IObj` can accept metadata."
  [x]
  (instance? clojure.lang.IObj x))

(defn meta-preserving-transform-map
  "Takes an `instaparse.core/transform` map and returns one that, in addition to
  applying the specified transformation, also attaches the non-terminal as
  metadata under the key `::node-type`."
  [transform-map]
  (reduce-kv (fn [m k f]
               (assoc m k (comp #(cond-> % (iobj? %) (vary-meta assoc ::node-type k))
                                f)))
             {}
             transform-map))

(def literal-transformations
  "An `instaparse.core/transform` transform map that transforms literals into
  their corresponding values."
  {:string edn/read-string
   :symbol edn/read-string
   :nat    edn/read-string
   :float  edn/read-string
   :int    edn/read-string})

(defn transform
  "Transforms an InferenceQL parse tree into a map with the same information, but
  in a format that is easier to work with. The output of this function is
  consumed by `execute`."
  [parse-tree]
  (let [all-transformations (meta-preserving-transform-map
                             (merge literal-transformations
                                    {:query            (map-transformer)
                                     :probability-of   (map-transformer)
                                     :density-of       (map-transformer)
                                     :column-selection (map-transformer :column)
                                     :generate-model   (map-transformer :target)
                                     :ordering         (map-transformer :column)
                                     :table-lookup     (map-transformer :table-name)
                                     :model-lookup     (map-transformer :model-name)
                                     :generated-table  (map-transformer)

                                     :name       keyword
                                     :table-name keyword
                                     :predicate  symbol

                                     :star       (constantly :star)
                                     :ascending  (constantly :ascending)
                                     :descending (constantly :descending)

                                     :selections     vector
                                     :conditions     vector
                                     :constraints    vector
                                     :variable-names vector
                                     :target         vector

                                     :binding hash-map}))]
    (insta/transform all-transformations parse-tree)))

;;; Selections

(defn events-clauses
  "Given a variable and a collection of events, returns a sequence of Datalog
  `:where` clauses that bind the values satisfying those events to the provided
  variable."
  [variable events]
  (let [row-var (genvar "row-events-")
        row-events (filterv keyword? events)
        row-clause (cond (= [:star] events) `[(d/pull ~'$ ~'[*]       ~entity-var) ~row-var]
                         (seq row-events)   `[(d/pull ~'$ ~row-events ~entity-var) ~row-var]
                         :else              `[(~'ground {})                        ~row-var])

        binding-sym (genvar "binding-events-")
        binding-events (transduce (filter map?) merge {} events)

        event-clause `[(~'ground ~binding-events) ~binding-sym]
        merge-clause `[(merge ~row-var ~binding-sym) ~variable]]
    [row-clause event-clause merge-clause]))

(defn density-selection-clauses
  "Returns the Datalog clauses for a density selection."
  [{:keys [target constraints model selection-name]}]
  (let [model (or model {:model-name :model})

        selection-name (or selection-name (keyword (gensym "density")))
        log-density-var (variable (str "log-" (name selection-name)))
        density-var (variable selection-name)

        model-var       (genvar "model-")
        target-var      (genvar "target-")
        constraints-var (genvar "constraints-")

        target-clauses      (events-clauses target-var      target)
        constraints-clauses (events-clauses constraints-var constraints)

        logpdf-clauses `[[(gpm/logpdf ~model-var ~target-var ~constraints-var) ~log-density-var]
                         [(math/exp ~log-density-var) ~density-var]]]
    {:name   [selection-name]
     :find   [density-var]
     :in     [model-var]
     :inputs [model]
     :where  (reduce into [target-clauses constraints-clauses logpdf-clauses])}))

(defn column-selection-clauses
  "Returns the `:find` and `:where` clauses for a `:column-selection` parse tree
  node as a map."
  [{:keys [column selection-name]}]
  (let [name (name (or selection-name column))
        variable (variable name)]
    {:name [(keyword name)]
     :find [variable]
     :where `[[(~'get-else ~'$ ~entity-var ~column :iql/no-value) ~variable]]}))

(defn selection-clauses
  [selection]
  (case (safe-get (meta selection) ::node-type)
    :column-selection (column-selection-clauses selection)
    :probability-of   (density-selection-clauses selection)
    :density-of       (density-selection-clauses selection)))

(defn selections-clauses
  [selections]
  (merge-with into {:where [[entity-var :iql/type :iql.type/row]]}
              (if (= [:star] selections)
                {:find `[[(~'pull ~entity-var [~'*]) ~'...]]}
                (->> selections
                     (map selection-clauses)
                     (apply merge-with into {:find [entity-var]})))))

;;; Conditions

(def condition-transformations
  {:presence-condition (fn [c] [[entity-var c '_]])
   :absence-condition  (fn [c] `[[(~'missing? ~'$ ~entity-var ~c)]])

   :and-condition (fn [cs1 cs2] `[(~'and ~@cs1 ~@cs2)])
   :or-condition (fn [cs1 cs2] `[(~'or ~@cs1 ~@cs2)])

   :equality-condition  (fn [c v] [[entity-var c v]])

   :predicate symbol
   :predicate-condition (fn [c p v]
                          (let [sym (genvar)]
                            [[entity-var c sym]
                             [(list (symbol "clojure.core" (name p)) sym v)]]))})

(defn condition-clauses
  "Returns a sequence of Datalog `:where` clauses for the conditions ."
  [conditions]
  (insta/transform condition-transformations conditions))

;;; Parsing

(def parse
  "An instaparse parser for IQL SQL queries. The grammar is inlined at macro
  expansion time so that it can be used in the ClojureScript context where we
  don't have access to file resources."
  (insta/parser (io/inline-file "inferenceql/query/grammar.bnf")
                :string-ci true))

;;; Query execution

(defn inputize
  "Modifies the provided query plan such that all the symbols that are in the
  default environment are provided as inputs."
  [query-plan]
  (let [replaced-symbols (->> (select-keys (:query query-plan) [:find :where])
                              (tree-seq coll? seq)
                              (filter (set (keys default-environment)))
                              (distinct))
        input-names (zipmap (keys default-environment)
                            (map input-symbols
                                 (vals default-environment)))]
    (-> query-plan
        (update-in [:query]     #(walk/postwalk-replace input-names %))
        (update-in [:query :in] into (map input-names replaced-symbols))
        (update-in [:inputs]    into (map (fn [sym] (vary-meta {:function-name sym} assoc ::node-type :function-lookup))
                                          replaced-symbols)))))

(defn query-plan
  "Given a query parse tree returns a query plan for the top-most query.
  Subqueries will not be considered and are handled in a different step by the
  interpreter. See `q` for details."
  [{:keys [selections source conditions limit] :or {source (with-meta {:table-name :data}
                                                             {::node-type :table-lookup})}}]
  (let [{sel-find :find sel-in :in sel-where :where sel-inputs :inputs} (selections-clauses selections)
        source (cond (not= :generated-table (safe-get (meta source) ::node-type))
                     source

                     (not limit)
                     (throw (ex-info "Cannot GENERATE without LIMIT" {}))

                     :else
                     (assoc source :limit limit))
        cond-where (mapcat condition-clauses conditions)]
    {:query {:find sel-find
             :in (into '[$] sel-in)
             :where (into sel-where cond-where)}
     :inputs (into [source] sel-inputs)}))

(defn iql-db
  "Converts a vector of maps into Datalog database that can be queried with `q`."
  [rows]
  (let [facts (map #(assoc % :iql/type :iql.type/row)
                   rows)]
    (d/db-with (d/empty-db) facts)))

(defn input
  "Turns an input description, a data description of an input to be provided to
  the Datalog query, into an actual input."
  [x environment]
  (case (get (meta x) ::node-type)
    :table-lookup    (get environment (:table-name x))
    :model-lookup    (get environment (:model-name x))
    :function-lookup (get environment (:function-name x))
    :generate-model  (let [model (if-let [model-desc (:model x)]
                                   (input model-desc environment)
                                   (safe-get environment :model))]
                       (constrain model
                                  (get x :target [])
                                  (get x :bindings {})))
    :generated-table (let [target (get-in x [:generate-model :target])
                           gfn (input (:generate-model x) environment)
                           limit (safe-get x :limit)]
                       (gpm/simulate gfn target {} limit))))

(defn execute
  "Like `q`, only operates on a query parse tree rather than a query string."
  ([parse-map rows]
   (execute parse-map rows {}))
  ([{:keys [selections ordering limit] :as parse-map} rows models]
   (let [environment (merge default-environment models {:data rows})
         keyfn (get ordering :column :db/id)
         cmp (case (get ordering :direction)
               :ascending compare
               :descending #(compare %2 %1)
               nil compare)
         names (-> selections (selections-clauses) (:name)) ; TODO: fix redundant call to selections-clauses
         {query :query input-descs :inputs} (inputize (query-plan parse-map))
         inputs (-> (mapv #(input % environment) input-descs)
                    (update 0 iql-db))
         rows (cond->> (apply d/q query inputs)
                names (map #(zipmap (into [:db/id] names) ; TODO: Can this not be hard-coded?
                                    %))
                :always (sort-by keyfn cmp)
                :always (map #(into {}
                                    (remove (comp #{:iql/no-value} val))
                                    %))
                :always (map #(dissoc % :db/id :iql/type))
                limit (take limit))
         metadata (or names
                      (into []
                            (comp (mapcat keys)
                                  (distinct))
                            rows))]
     (vary-meta rows assoc :iql/columns metadata))))

(defn q
  "Returns the result of executing a query on a set of rows. A registry
  mapping model names to model values models can be provided as an optional
  third argument."
  ([query rows]
   (q query rows {}))
  ([query rows models]
   (let [result (parse query)]
     (if-not (insta/failure? result)
       (execute (transform result) rows models)
       (let [failure (insta/get-failure result)
             ex-map {:cognitect.anomalies/category :cognitect.anomalies/incorrect
                     :inferenceql.query.instaparse/failure failure}]
         (throw (ex-info "Parsing failure" ex-map)))))))
