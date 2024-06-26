(ns gensql.query.literal
  (:refer-clojure :exclude [read])
  (:require [clojure.core.match :as match]
            [clojure.edn :as edn]
            [gensql.query.parser.tree :as tree]
            [gensql.query.relation :as relation]))

(defn read
  "Returns Clojure data representing a parse node.
  Can be bools, ints, floats, strings, lists, etc."
  [node]
  (match/match [(into (empty node)
                      (remove tree/whitespace?)
                      node)]
    [[:value child]] (read child)

    [[:bool s]]              (edn/read-string s)
    [[:float s]]             (edn/read-string s)
    [[:int s]]               (edn/read-string s)
    [[:nat s]]               (edn/read-string s)
    [[:identifier child]]    (read child)
    [[:variable _var child]] (read child)
    [[:simple-symbol s]]     (edn/read-string (str \" s \"))
    [[:delimited-symbol s]]  (edn/read-string (str \" s \"))
    [[:string s]]            (edn/read-string (str \" s \"))

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
