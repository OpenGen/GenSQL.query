(ns inferenceql.query.model
  (:require [clojure.spec.alpha :as s]))

(s/def ::variable symbol?)
