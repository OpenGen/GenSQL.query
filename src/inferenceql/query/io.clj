(ns inferenceql.query.io
  (:import [tech.tablesaw.api Row]
           [tech.tablesaw.api Table])
  (:require [clojure.java.io :as io]
            [inferenceql.inference.gpm :as gpm]))

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
