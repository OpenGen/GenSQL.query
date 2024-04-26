(ns gensql.query.cache
  "For caching expensive results."
  (:require #?(:clj [clojure.core.memoize :as memo]
               :cljs ["memoizee" :as memoizee])
            [clojure.string :as string]))

(def default-threshold 100)


(defn lru
  "Memoizes a fn with a least-recently-used eviction policy.

  After the number of cached results exceeds the threshold, the
  least-recently-used ones will be evicted."
  ([f]
   (lru f default-threshold))
  ([f lru-threshold]
   #?(:clj (memo/lru f :lru/threshold lru-threshold)
      :cljs (memoizee f #js {"max" lru-threshold
                             "normalizer" js/JSON.stringify}))))
