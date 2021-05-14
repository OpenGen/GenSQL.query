(ns inferenceql.query.data
  "Functions for manipulating InferenceQL datasets."
  (:require [clojure.edn :as edn]
            [clojure.string :as string]))

(defn value-coercer
  "Returns a function that will attempt to coerce a value to a data type
  compatible with the given statistical type."
  [stattype]
  (let [coerce (case stattype
                  :nominal str
                  :numerical (comp double edn/read-string))]
    (fn [value]
      (when-not (string/blank? value)
        (coerce value)))))

(defn row-coercer
  "Returns a function that will attempt to coerce the values in a map to values
  that match on the statistical types provided."
  [variable->stattype]
  (fn [row]
    (reduce-kv (fn [row variable stattype]
                 (update row variable (value-coercer stattype)))
               row
               variable->stattype)))
