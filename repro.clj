(require
 '[inferenceql.query.db :as db]
 '[inferenceql.query.permissive :as permissive]
 '[inferenceql.query.io :as io]
 ;; '[inferenceql.gpm.sppl :as sppl]
 )


(def database (-> (db/empty)
                  (db/with-table :data (io/slurp-csv "d1.csv"))
                  (db/with-table :data_with_missing (io/slurp-csv "d2.csv"))
                  ;; (db/with-model :model_spe (sppl/slurp "SPE.json"))
                  (db/with-model :model_clojurecat (io/slurp-model "ClojureCat.edn"))
                  atom))

(comment

  (println "Verify that both models work fine.")
  (prn (permissive/query "SELECT * FROM GENERATE * UNDER model_spe LIMIT 1" database))
  (prn (permissive/query "SELECT * FROM GENERATE * UNDER model_clojurecat LIMIT 1" database))
  (println "==================")
  (println " ")

  (println "==== SPPL ======")
  (println "This is working fine:")
  (prn (permissive/query "(SELECT bar FROM data) GENERATIVE JOIN model_spe GIVEN bar" database))
  (println " ")
  (println "This is not.")
  (prn (permissive/query "(SELECT bar FROM data_with_missing) GENERATIVE JOIN model_spe GIVEN bar" database))
  (println " ")

  (println "==== ClojureCat ======")
  (println "Same error with ClojureCAt")
  (prn (permissive/query "(SELECT bar FROM data_with_missing) GENERATIVE JOIN model_clojurecat GIVEN bar" database))


  ;; And this is breaking for yet another silly reason. Namely, Nick copied over assertions from Python-CGPM.
  (prn (permissive/query "(SELECT bar FROM data) GENERATIVE JOIN model_clojurecat GIVEN bar" database))

  ,)
