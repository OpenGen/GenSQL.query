(ns gensql.query.cli
  "Contains general CLI helper fns")

(defn parse-named-pair
  "Parses strings like \"FOO=BAR\". Returns a map of the key and value."
  [s]
  (when-let [[_ name path] (re-matches #"([^=]+)=([^=]+)" s)]
    {name path}))

