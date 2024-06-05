(ns gensql.query.perf.synthetic
  "For generating artificial data and models, of varying ecological validity."
  (:require [clojure.math :as math]
            [gensql.inference.gpm.crosscat :as xcat]
            [gensql.query.db :as db]
            [medley.core :as medley]))

(def ^:const view-prefix "view-")
(def ^:const col-prefix "col-")

(def ^:dynamic *default-categorical-alpha* 0.01)
(def ^:dynamic *default-local-alpha* 0.01)
(def ^:dynamic *default-global-alpha* 0.01)
(def ^:dynamic *default-m* 1)
(def ^:dynamic *default-r* 2)
(def ^:dynamic *default-s* 3)
(def ^:dynamic *default-nu* 4)

(def ^:dynamic *default-table-name* "table")
(def ^:dynamic *default-model-name* "model")

(defn num-digits-needed
  "How many digits are needed to represent the largest number in the data.
  Use for formatting names without too many zeros all over the place."
  [max-n]
  (int (math/ceil (math/log10 max-n))))

(defn numbered-format
  "Returns a numbered format string for strings like \"prefix-00\"."
  [prefix n]
  (str prefix "%0" (num-digits-needed n) "d"))

(defn ^:private name-vec
  "Generates a vector of numbered strings with a prefix, like (\"view-1\",
  \"view-2\", \"view-3\" ...)"
  [prefix num-names]
  (let [name-format (numbered-format prefix num-names)]
    (mapv #(format name-format %) (range num-names))))

(defn generate-data
  "Generates a synthetic dataset with the given number of rows and columns.

  The p-categorical is the proportion that are categorical, and the rest are
  gaussian. Note that this is an enforced proportion, not a probability.

  For categories, generates between 2-10 values in each category.

  Returns a map with the data and the column-type metadata"
  [num-rows num-columns p-categorical]
  (let [col-format (numbered-format col-prefix num-columns)
        col-names (name-vec col-prefix num-columns)
        generate-category-values (fn [col-index]
                                   ;; Generate 2 to 10 category vals
                                   (let [num-category-vals (+ 2 (rand-int 8))]
                                     (name-vec "val-" num-category-vals)))
        column-types (let [num-categorical-cols (int (math/round (* num-columns p-categorical)))]
                       (->> (range num-columns)
                            (map #(if (< % num-categorical-cols)
                                    {:type   :categorical
                                     :values (generate-category-values %)}
                                    {:type :gaussian}))
                            shuffle
                            (map-indexed #(assoc %2 :name (format col-format %1)))
                            vec))
        generate-value (fn [{:keys [type values]}]
                         (case type
                           ; not really gaussian, but shouldn't matter for benchmarks
                           :gaussian (rand-int 100)
                           :categorical (rand-nth values)))
        generate-row (fn []
                       (zipmap col-names
                               (map generate-value column-types)))
        data (vec (repeatedly num-rows generate-row))]
    {:data         data
     :column-types column-types}))


(defn ^:private partition-evenly
  "Splits xs into n groups of m (or m+1) elements, so each group is as close in
  size as possible.

  Relative order is not guaranteed."
  [n xs]
  (assert (<= n (count xs)))
  (let [m (int (math/floor (/ (count xs) n)))
        num-remaining (mod (count xs) n)
        [groups remaining] (split-at (* m n) xs)
        [groups-to-enlarge finished-groups] (split-at num-remaining (partition m groups))]
    (into (map conj groups-to-enlarge remaining)
          finished-groups)))


(defn spec-views
  "Creates XCat views for the spec.

  Evenly splits the columns across views. Not realistic, but better for
  consistent benchmarking."
  [num-views column-types {:keys [categorical-alpha m r s nu]}]
  (let [categorical-hypers {:alpha categorical-alpha}
        gaussian-hypers {:m m :r r :s s :nu nu}
        hyper-groups (->> column-types
                          (partition-evenly num-views)
                          (map (fn [hg]
                                 (reduce (fn [hypers col-type]
                                           (assoc hypers
                                                  (:name col-type)
                                                  (if (= :categorical (:type col-type))
                                                    categorical-hypers
                                                    gaussian-hypers)))
                                         {}
                                         hg))))]
    (zipmap (name-vec view-prefix num-views)
            (map #(array-map :hypers %) hyper-groups))))


(defn latents-local
  "Creates XCat latents for the model"
  [data num-views num-clusters-per-view alpha]
  (let [data-size (count data)
        view-names (name-vec view-prefix num-views)
        cluster-names (name-vec "clust-" num-clusters-per-view)
        clusters (zipmap cluster-names
                         (partition-evenly num-clusters-per-view (range data-size)))]
    (reduce
      (fn [local view-name]
        (assoc local
               view-name
               {:alpha  alpha
                :counts (medley/map-vals count clusters)
                :y      (reduce-kv (fn [y cl-name row-nums]
                                     (reduce #(assoc %1 %2 cl-name)
                                             y
                                             row-nums))
                                   {}
                                   clusters)}))
      {}
      view-names)))


(defn generate-model
  "Generates a CrossCat model from the given data and options.

  NB: only categorical and gaussian primitives are currently supported."
  [data column-types num-views num-clusters-per-view {:keys [local-alpha global-alpha] :as opts}]
  (assert (<= num-clusters-per-view (count data)))
  (let [categorical-cols (filter #(= :categorical (:type %)) column-types)
        spec-col-types (zipmap (map :name column-types)
                               (map :type column-types))
        options (zipmap (map :name categorical-cols)
                        (map #(-> % :values vec) categorical-cols))

        xcat-spec {:views (spec-views num-views column-types opts)
                   :types spec-col-types}
        xcat-latents {:global {:alpha global-alpha}
                      :local  (latents-local data num-views num-clusters-per-view local-alpha)}]

    (xcat/construct-xcat-from-latents xcat-spec xcat-latents data {:options options})))


(defn generate-db
  "Generates a completely synthetic data set and XCat model, and returns a
  valid query database.

  Strictly for benchmarking. The data is largely random, and the model is not
  actually trained from it.

  Example usage:
  (def db (generate-db {:num-rows              100
                        :num-columns           10
                        :p-categorical         0.6
                        :num-views             3
                        :num-clusters-per-view 2}))"
  [{:keys [num-rows
           num-columns
           p-categorical
           num-views
           num-clusters-per-view
           m
           r
           s
           nu
           categorical-alpha
           local-alpha
           global-alpha]
    :as   opts}]
  (assert (<= num-clusters-per-view num-rows))
  (assert (<= num-views num-columns))
  (let [opts (merge {:categorical-alpha *default-categorical-alpha*
                     :local-alpha       *default-local-alpha*
                     :global-alpha      *default-global-alpha*
                     :m                 *default-m*
                     :r                 *default-r*
                     :s                 *default-s*
                     :nu                *default-nu*}
                    opts)
        {:keys [data column-types]} (generate-data num-rows num-columns p-categorical)
        model (generate-model data
                              column-types
                              num-views
                              num-clusters-per-view
                              opts)]
    (-> (db/empty)
        (db/with-table *default-table-name* data)
        (db/with-model *default-model-name* model)
        (assoc :synthetic/column-types column-types))))
