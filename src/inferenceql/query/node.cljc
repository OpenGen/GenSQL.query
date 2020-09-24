(ns inferenceql.query.node)

(defn tag
  [node]
  (first node))

(defn whitespace?
  [node]
  (contains? #{:ws :comma} (tag node)))
