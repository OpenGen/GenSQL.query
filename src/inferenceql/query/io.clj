(ns inferenceql.query.io)

(defmacro inline-file
  "Inlines the contents of the named resource as a string."
  [n]
  (slurp n))
