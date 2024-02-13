(ns inferenceql.query.literal
  (:refer-clojure :exclude [read])
  (:require [clojure.core.match :as match]
            [clojure.edn :as edn]
            [inferenceql.query.parser.tree :as tree]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.string :as q.string]))

(defn read
  "Recursively walks a parse tree and replaces nodes of literals with their
  Clojure equivalents."
  [node]
  (match/match [(into (empty node)
                      (remove tree/whitespace?)
                      node)]
    [[:value child]] (read child)

    [[:bool s]]             (edn/read-string s)
    [[:float s]]            (edn/read-string s)
    [[:int s]]              (edn/read-string s)
    [[:nat s]]              (edn/read-string s)
    [[:simple-symbol s]]    (edn/read-string s)
    [[:delimited-symbol s]] (edn/read-string (q.string/safe-symbol s))
    [[:string s]]           (edn/read-string (str \" s \"))

    [[:null _]] nil
    [nil] nil

    [[:relation-expr child]] (read child)
    [[:relation-expr child _semicolon]] (read child)
    [[:value-lists child]] (read child)

    [[:identifier-list & children]]
    (map read (filter tree/branch? children))

    [[:value-lists-full & children]]
    (map read (filter tree/branch? children))

    [[:value-lists-sparse & children]]
    (let [pairs (->> children
                     (filter tree/branch?)
                     (map read)
                     (partition 2))
          n (inc (apply max (map first pairs)))]
      (reduce #(apply assoc %1 %2)
              (vec (repeat n ()))
              pairs))

    [[:value-list & children]]
    (map read (filter tree/branch? children))

    [[:relation-value _open-paren syms _close-paren _values vals]]
    (let [attrs (read syms)
          ms (map #(zipmap attrs %)
                  (read vals))]
      (relation/relation ms :attrs attrs))))
