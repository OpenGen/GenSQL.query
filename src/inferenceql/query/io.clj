(ns inferenceql.query.io
  (:import [tech.tablesaw.api Row]
           [tech.tablesaw.api Table]
           [tech.tablesaw.io.csv CsvReadOptions])
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.string :as query.string]))

(set! *warn-on-reflection* true)

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
  (let [csv-string (slurp x)
        ^Table table (-> (Table/read)
                         (.usingOptions (-> (CsvReadOptions/builderFromString csv-string)
                                            (.sample false))))
        columns (.columnNames table)
        attrs (map query.string/safe-symbol columns)
        row (Row. table)
        coll (loop [i 0
                    rows (transient [])]
               (if (>= i (.rowCount table))
                 (persistent! rows)
                 (do (.at row i)
                     (let [row (reduce (fn [m ^String column]
                                         (let [value (.getObject row column)
                                               attr (query.string/safe-symbol column)]
                                           (cond-> m
                                             (not (contains? #{"" nil} value))
                                             (assoc attr value))))
                                       {}
                                       columns)]
                       (recur (inc i) (conj! rows row))))))]
    (relation/relation coll :attrs attrs)))

(defn spit-csv
  "Attempts to write a collection to a location. x is coerced to a writer as per
  `io/writer`."
  [rel x]
  (let [coll (relation/->vector rel)
        [attr-row & tuple-rows] coll
        csv-data (into [(mapv query.string/demunge attr-row)]
                       tuple-rows)]
    (with-open [writer (io/writer x)]
      (csv/write-csv writer csv-data))))
