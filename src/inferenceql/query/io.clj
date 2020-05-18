(ns inferenceql.query.io
  (:require [clojure.java.io :as io]))

(defmacro inline-file
  "Inlines the contents of the named resource as a string."
  [n]
  (slurp (io/resource n)))
