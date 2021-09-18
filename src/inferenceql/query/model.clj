(ns inferenceql.query.model
  (:refer-clojure :exclude [eval])
  (:require [inferenceql.query.environment :as env]))

(defn lookup-op
  [model-name]
  {:operation/type :operation.type/model-lookup
   :operation/model-name model-name})

(defmulti eval (fn [node _env] (:operation/type node)))

(defmethod eval :operation.type/model-lookup
  [op env]
  (let [{:operation/keys [model-name]} op]
    (env/get env model-name)))

