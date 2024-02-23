(ns inferenceql.query.log)

(defn spy-tap>
  "Like tap>, but returns what's passed in, so can be used inline.

  To help identify the tap, it places the value in a map namespaced with `spy`."
  [x]
  (tap> #:spy{:x x})
  x)
