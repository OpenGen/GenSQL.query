(ns gensql.query.node)

(defn tag
  [node]
  (first node))

(defn has-tag?
  [node t]
  (and (vector? node) ; FIXME: this is a redefinition of tree/branch?
       (= t (tag node))))

(defn whitespace?
  [node]
  (contains? #{:ws :comma} (tag node)))
