(ns gensql.query.log)

(defn spy-tap>
  "Like tap>, but returns what's passed in, so can be used inline.

  To help identify the tap, it places the value in a map namespaced with `spy`."
  [x]
  (tap> #:spy{:x x})
  x)

(defn log-tap
  "A tap that prns the value to the console. Use `(add-tap log-tap)` to add it."
  [x]
  (prn x)
  (println))

(defn clear-taps
  "Clears all taps.

  This is primarily for removing anonymous tap fns. If you don't have a reference
  to the tap fn, you can't remove it. But if you do, prefer `remove-tap`."
  []
  ;; tapset is annoyingly private (and Clojure lacks this functionality), so we
  ;; have to use intern to bypass that
  (intern 'clojure.core 'tapset (atom #{})))
