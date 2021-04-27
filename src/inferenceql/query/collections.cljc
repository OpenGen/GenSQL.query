(ns inferenceql.query.collections)

(defn safe-get
  "Like `clojure.core/get`, but throws and exception if k is not in coll."
  [coll k]
  (if (contains? coll k)
    (get coll k)
    (throw (ex-info "Collection does not contain key"
                    {::error ::safe-get
                     ::coll coll
                     ::k k}))))

(defn all-keys
  "Returns a vector of all the distinct keys in a collection of associative data
  structures."
  [ms]
  (into []
        (comp (mapcat keys)
              (distinct))
        ms))
