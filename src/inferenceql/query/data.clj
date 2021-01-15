(ns inferenceql.query.data
  "Functions for manipulating InferenceQL datasets."
  (:require [clojure.edn :as edn]
            [clojure.string :as string]))

(defn value-coercer
  "Returns a function that will attempt to coerce a value to a data type
  compatible with the given statistical type."
  [stattype]
  (let [coerce (case stattype
                  :binary (comp boolean edn/read-string)
                  :categorical str
                  :gaussian (comp double edn/read-string))]
    (fn [value]
      (when-not (string/blank? value)
        (coerce value)))))

(defn row-coercer
  "Returns a function that will attempt to coerce the values in a map to values
  that match on the statistical types provided."
  [variable->stattype]
  (reduce-kv (fn [coercers variable stattype]
               (fn [row]
                 (-> row
                     (update variable (value-coercer stattype))
                     (coercers))))
             identity
             variable->stattype))
