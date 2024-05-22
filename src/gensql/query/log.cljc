(ns gensql.query.log
  (:require [clojure.repl :as repl]))

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

(defn print-exception
  [e]
  (binding [*out* *err*
            *print-length* 10
            *print-level* 4]
    (if-let [parse-failure (:instaparse/failure (ex-data e))]
      (print parse-failure)
      (if-let [ex-message (ex-message e)]
        (println ex-message)
        (repl/pst e)))))

(defn errorln
  "Like `clojure.core/println`, but prints to `clojure.core/*err*` instead of
  `clojure.core/*out*`."
  [& args]
  (binding [*out* *err*]
    (apply println args)))
