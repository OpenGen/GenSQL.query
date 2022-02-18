(ns inferenceql.query.statement-test
  (:refer-clojure :exclude [alter eval update])
  (:require [clojure.test :refer [are deftest is]]
            ;; [inferenceql.query.db :as db]
            [inferenceql.query.parser :as parser]
            [inferenceql.query.relation :as relation]
            [inferenceql.query.statement :as statement]))

(deftest drop-table-statement?
  (are [s] (statement/statement-node? (parser/parse s))
    "DROP TABLE data!"
    "DROP TABLE IF EXISTS data!"))

(deftest insert-statement?
  (are [s] (statement/statement-node? (parser/parse s))
    "INSERT INTO data (x) VALUES (0)!"))

(deftest update-statement?
  (are [s] (statement/statement-node? (parser/parse s))
    "UPDATE data SET x = 3!"
    "UPDATE data SET x = 3 WHERE y = 2!"))

(deftest update-statement?
  (are [s] (statement/statement-node? (parser/parse s))
    "UPDATE data SET x = 3!"))

(defn eval
  [tables stmt]
  (let [db {:iql/tables tables}
        f (statement/eval (parser/parse stmt))
        db (f db)]
    (:iql/tables db)))

(deftest drop-table
  (are [before stmt after] (= after (eval before stmt))
    '{data []} "DROP TABLE data!" '{}
    '{data []} "DROP TABLE IF EXISTS data!" '{}
    '{} "DROP TABLE IF EXISTS data!" '{}))

(deftest drop-table-error
  (is (thrown? Exception (eval {} "DROP TABLE data!"))))

(deftest insert-into
  (are [before stmt after] (= after (eval before stmt))
    '{data []} "INSERT INTO data (x) VALUES (0)!" '{data [{x 0}]}
    '{data [{x 0}]} "INSERT INTO data (x) VALUES (1)!" '{data [{x 0} {x 1}]}
    '{data []} "INSERT INTO data (x) VALUES (0), (1), (2)!" '{data [{x 0} {x 1} {x 2}]}))

(deftest insert-into-error
  (is (thrown? Exception (eval {} "DROP TABLE data!"))))

(deftest update
  (are [before stmt after] (= after (eval before stmt))
    '{data [{x 0}]} "UPDATE data SET x = 1!" '{data [{x 1}]}
    '{data [{x 0} {x 1} {x 2}]} "UPDATE data SET x = 1!" '{data [{x 1} {x 1} {x 1}]}
    '{data [{x 0} {x 1} {x 2}]} "UPDATE data SET x = 3 WHERE x = 1!" '{data [{x 0} {x 3} {x 2}]}))

(deftest alter
  (are [before stmt after] (let [in-rel (relation/relation [] :attrs before :name 'data)
                                 out-rel (get (eval {'data in-rel} stmt)
                                              'data)]
                             (= after (relation/attributes out-rel)))
    '[] "ALTER data ADD x!" '[x]
    '[x] "ALTER data ADD y!" '[x y]))


(comment

  (parser/parse "UPDATE data SET x = 3!")
  (parser/parse "ALTER data ADD y!")

  ,)
