(ns inferenceql.query.datalog
  (:refer-clojure :exclude [merge])
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [datascript.core :as d]))

(defn merge
  "Merges two queries, preserving the clauses from both. Eliminates top-level
  duplicates in the `:where` clause."
  [& qs]
  (reduce (fn [q1 q2]
            (update (merge-with #(into (vec %1) %2)
                                q1
                                q2)
                    :where
                    #(into [] (distinct) %)))
          {}
          qs))

(defn find-clause
  "Returns the find clauses in a Datalog query."
  [query]
  (->> query
       (drop-while (complement #{:find}))
       (take-while (complement #{:in :where}))
       (rest)))

(defn in-clause
  "Returns the inputs from a Datalog query."
  [query]
  (->> query
       (drop-while (complement #{:in}))
       (rest)
       (take-while (complement #{:where}))))

(defn where-clauses
  "Returns the where clauses in a Datalog query."
  [query]
  (->> query
       (drop-while (complement #{:where}))
       (rest)))

(defn variable?
  "Returns true if `sym` is a Datalog variable."
  [sym]
  (and (simple-symbol? sym)
       (string/starts-with? (name sym) "?")))

(defn free-variables
  "Returns the variables anywhere in the provided Datalog form."
  [form]
  (->> (tree-seq seqable? seq form)
       (filter variable?)
       (distinct)))

(defn tuple-get
  "Given a tuple and a vector of named keys for the elements of that tuple,
  returns the value for the provided key."
  [ks tuple k]
  (get (zipmap ks tuple) k))

(defprotocol Extension
  "A Datalog extension."
  (matches? [this clause]
    "Returns true if `clause` is an instance of this extension.")
  (symbols [this clause]
    "Returns a map with two keys: The value for `:arg-syms` is the symbols
    needed to execute this extension. The value for `:out-syms` is the variable
    bindings produced by this extension.")
  (execute [this]
    "Returns a function that, when called on a relation tuple, either returns
    `nil` if that tuple should be removed, or a new tuple with new values
    corresponding to the `:out-syms` provided by `symbols`."))

(defn execute-extension
  "Executes a custom extension on a custom clause using the provided inputs and
  relation. Returns a new relation and associated variable names."
  [extension clause non-binding-syms non-binding-inputs vars relation]
  (let [{:keys [arg-syms out-syms]} (symbols extension clause)]
    (assert (empty? (set/intersection (set vars) (set out-syms))))
    {:vars (into (vec vars) out-syms)
     :relation (keep (fn [tuple]
                       (let [execute (execute extension)
                             args (for [sym arg-syms]
                                    (if (variable? sym)
                                      (tuple-get vars tuple sym)
                                      (tuple-get non-binding-syms non-binding-inputs sym)))]
                         (when-let [new-tuple (apply execute args)]
                           (into (vec tuple) new-tuple))))
                     relation)}))

(defn q
  "Like `datascript.core/q`, except it can handle custom Datalog extensions.
  Extensions must implement `Extension`."
  [query extensions & inputs]
  (let [clause-extension (fn [clause]
                           (->> extensions
                                (filter #(matches? % clause))
                                (first)))
        in-clause (in-clause query)
        [datalog-clauses [custom-clause & more-clauses]] (split-with (comp nil? clause-extension)
                                                                     (where-clauses query))]
    (if (empty? custom-clause)
      (apply d/q query inputs)
      (let [vars (distinct (into (free-variables in-clause)
                                 (free-variables datalog-clauses)))
            non-binding-syms (filter (every-pred symbol? (complement variable?))
                                     in-clause)
            non-binding-inputs (mapv (zipmap in-clause inputs)
                                     non-binding-syms)
            relation (apply d/q
                            {:find vars :in in-clause :where datalog-clauses}
                            inputs)

            extension (clause-extension custom-clause)
            {new-vars :vars new-relation :relation} (execute-extension extension
                                                                       custom-clause
                                                                       non-binding-syms
                                                                       non-binding-inputs
                                                                       vars
                                                                       relation)

            next-query (into `[:find ~@(find-clause query)
                               :in ~@non-binding-syms [[~@new-vars]]]
                             (when (seq more-clauses)
                               `[:where ~@more-clauses]))

            next-inputs (conj non-binding-inputs new-relation)]
        (apply q next-query extensions next-inputs)))))
