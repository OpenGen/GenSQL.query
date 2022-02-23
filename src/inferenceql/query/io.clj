(ns inferenceql.query.io
  (:import [tech.tablesaw.api Row]
           [tech.tablesaw.api Table])
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.relation :as relation]))

(defmacro inline-file
  "Inlines the contents of the named resource as a string."
  [n]
  (slurp (io/resource n)))

(defn slurp-model
  "Opens a reader on x, reads its contents, parses its contents into EDN
  specification, and from that specification creates a multimixture model. See
  `clojure.java.io/reader` for a complete list of supported arguments."
  [x]
  (-> (slurp x) (gpm/read-string)))

(defn slurp-csv
  "Opens a reader on x, reads its contents, parses its contents as a table, and
  then converts that table into a relation. See `clojure.java.io/reader` for a
  complete list of supported arguments."
  [x]
  (let [^Table table (.csv (Table/read) (slurp x) "")
        columns (.columnNames table)
        attrs (map symbol columns)
        row (Row. table)
        coll (loop [i 0
                    rows (transient [])]
               (if (>= i (.rowCount table))
                 (persistent! rows)
                 (do (.at row i)
                     (let [row (zipmap attrs
                                       (map #(.getObject row %)
                                            columns))]
                       (recur (inc i) (conj! rows row))))))]
    (with-meta coll {:iql/columns attrs})))

(defn spit-csv
  "Attempts to write a collection to a location. x is coerced to a writer as per
  `io/writer`."
  [rel x]
  (let [coll (relation/->vector rel)]
    (with-open [writer (io/writer x)]
      (csv/write-csv writer coll))))

(comment

  (let [rel (relation/relation '[{x 0 y 2} {x 1 y 1} {x 2 y 0}] :attrs '[x y])]
    (spit-csv rel "/Users/zane/Desktop/test.csv"))

  )
