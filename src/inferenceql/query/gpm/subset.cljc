(ns inferenceql.query.gpm.subset
  (:require [clojure.set :as set]
            [inferenceql.inference.gpm :as gpm]
            [inferenceql.inference.gpm.proto :as gpm.proto]))

(defrecord SubsetGPM [gpm variables]
  gpm.proto/GPM
  (logpdf [_ targets conditions]
    (let [targets (select-keys targets variables)]
      (gpm/logpdf gpm targets conditions)))

  (simulate [_ targets conditions]
    (let [targets (set/intersection (set variables) (set targets))]
      (gpm/simulate gpm targets conditions)))

  gpm.proto/Variables
  (variables [_]
    (set/intersection (set variables) (set (gpm/variables gpm)))))

(defn subset-gpm
  "Returns a GPM that only supports the provided variables."
  [gpm variables]
  (->SubsetGPM gpm variables))
