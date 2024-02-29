(ns inferenceql.query.scalar-test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer [are deftest is testing]]
            #?(:clj [inferenceql.inference.gpm.conditioned :as conditioned]
               :cljs [inferenceql.inference.gpm.conditioned :as conditioned :refer [ConditionedGPM]])
            #?(:clj [inferenceql.inference.gpm.constrained :as constrained]
               :cljs [inferenceql.inference.gpm.constrained :as constrained :refer [ConstrainedGPM]])
            [inferenceql.inference.gpm.proto :as gpm.proto]
            [inferenceql.query.scalar :as scalar]
            [inferenceql.query.strict.parser :as parser]
            [inferenceql.query.tuple :as tuple])
  #?(:clj (:import [inferenceql.inference.gpm.conditioned ConditionedGPM]
                   [inferenceql.inference.gpm.constrained ConstrainedGPM])))

(defn plan
  [s & {:keys [start] :or {start :scalar-expr}}]
  (-> (parser/parse s :start start)
      (scalar/plan)))

(deftest plan-identifier
  (is (= '(iql/safe-get iql-bindings "x") (plan "x")))
  (is (= '(iql/safe-get iql-bindings "x") (plan "\"x\"")))

  (testing "delimited"
    (let [s "!@#$%^&*"]
      (is (= (list 'iql/safe-get 'iql-bindings s) (plan (str \" s \")))))

    (are [s] (= `(~'iql/safe-get ~'iql-bindings ~s)  (plan (str \" s \")))
      "x"
      " oddity"
      "mazzy*"
      "$parton"
      "|bie-girl"
      "()s just don't understand"
      "king of ^ flowers"
      "!@{}[]#$%^&*()_")))

(deftest plan-operator
  (are [s sexpr] (= sexpr (plan s))
    "x or y" '(or (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y"))
    "x and y" '(and (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y"))
    "not x" '(not (iql/safe-get iql-bindings "x"))
    "x > y" '(> (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y"))
    "x >= y" '(>= (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y"))
    "x = 0" '(= (iql/safe-get iql-bindings "x") 0)
    "x <= y" '(<= (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y"))
    "x < y" '(< (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y"))
    "x + y" '(+ (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y"))
    "x - y" '(- (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y"))
    "x * y" '(* (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y"))
    "x / y" '(/ (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")))

  (testing "delimited interaction"
    (are [s sexpr] (= sexpr (plan s))
      "\"~x\" or y" '(or (iql/safe-get iql-bindings "~x") (iql/safe-get iql-bindings "y"))
      "\"~x\" and y" '(and (iql/safe-get iql-bindings "~x") (iql/safe-get iql-bindings "y"))
      "not \"~x\"" '(not (iql/safe-get iql-bindings "~x"))
      "\"~x\" > y" '(> (iql/safe-get iql-bindings "~x") (iql/safe-get iql-bindings "y"))
      "\"~x\" >= y" '(>= (iql/safe-get iql-bindings "~x") (iql/safe-get iql-bindings "y"))
      "\"~x\" = 0" '(= (iql/safe-get iql-bindings "~x") 0)
      "\"~x\" <= y" '(<= (iql/safe-get iql-bindings "~x") (iql/safe-get iql-bindings "y"))
      "\"~x\" < y" '(< (iql/safe-get iql-bindings "~x") (iql/safe-get iql-bindings "y"))
      "\"~x\" + y" '(+ (iql/safe-get iql-bindings "~x") (iql/safe-get iql-bindings "y"))
      "\"~x\" - y" '(- (iql/safe-get iql-bindings "~x") (iql/safe-get iql-bindings "y"))
      "\"~x\" * y" '(* (iql/safe-get iql-bindings "~x") (iql/safe-get iql-bindings "y"))
      "\"~x\" / y" '(/ (iql/safe-get iql-bindings "~x") (iql/safe-get iql-bindings "y")))))

(deftest plan-operator-precedence-same
  (are [s sexpr] (= sexpr (plan s))
    "x or y or z" '(or (or (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x and y and z" '(and (and (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x + y + z" '(+ (+ (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x - y - z" '(- (- (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x * y * z" '(* (* (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x / y / z" '(/ (/ (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x > y > z" '(> (> (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x >= y >= z" '(>= (>= (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x = y = z" '(= (= (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x <= y <= z" '(<= (<= (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x < y < z" '(< (< (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x is y is z" '(= (= (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "\"x \" is \"y \" is \"z \"" '(= (= (iql/safe-get iql-bindings "x ") (iql/safe-get iql-bindings "y ")) (iql/safe-get iql-bindings "z "))))

(deftest plan-operator-precedence-different
  (are [s sexpr] (= sexpr (plan s))
    "x and y or z" '(or (and (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x or y and z" '(or (iql/safe-get iql-bindings "x") (and (iql/safe-get iql-bindings "y") (iql/safe-get iql-bindings "z")))
    "not x and y" '(and (not (iql/safe-get iql-bindings "x")) (iql/safe-get iql-bindings "y"))
    "not x or y" '(or (not (iql/safe-get iql-bindings "x")) (iql/safe-get iql-bindings "y"))
    "not x = y" '(not (= (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")))
    "x = y + z" '(= (iql/safe-get iql-bindings "x") (+ (iql/safe-get iql-bindings "y") (iql/safe-get iql-bindings "z")))
    "x + y = z" '(= (+ (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x + y * z" '(+ (iql/safe-get iql-bindings "x") (* (iql/safe-get iql-bindings "y") (iql/safe-get iql-bindings "z")))
    "x * y + z" '(+ (* (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "\" x\" * y + z" '(+ (* (iql/safe-get iql-bindings " x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))))


(deftest plan-operator-group
  (are [s sexpr] (= sexpr (plan s))
    "x and (y or z)" '(and (iql/safe-get iql-bindings "x") (or (iql/safe-get iql-bindings "y") (iql/safe-get iql-bindings "z")))
    "(x or y) and z" '(and (or (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "(not x) and y" '(and (not (iql/safe-get iql-bindings "x")) (iql/safe-get iql-bindings "y"))
    "(not x) or y" '(or (not (iql/safe-get iql-bindings "x")) (iql/safe-get iql-bindings "y"))
    "(not x) = y" '(= (not (iql/safe-get iql-bindings "x")) (iql/safe-get iql-bindings "y"))
    "(x = y) + z" '(+ (= (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x + (y = z)" '(+ (iql/safe-get iql-bindings "x") (= (iql/safe-get iql-bindings "y") (iql/safe-get iql-bindings "z")))
    "(x + y) * z" '(* (+ (iql/safe-get iql-bindings "x") (iql/safe-get iql-bindings "y")) (iql/safe-get iql-bindings "z"))
    "x * (y + z)" '(* (iql/safe-get iql-bindings "x") (+ (iql/safe-get iql-bindings "y") (iql/safe-get iql-bindings "z")))
    "\"x%\" * (y + z)" '(* (iql/safe-get iql-bindings "x%") (+ (iql/safe-get iql-bindings "y") (iql/safe-get iql-bindings "z")))))


(deftest plan-distribution-event
  (are [s sexpr] (= sexpr (plan s :start :distribution-event))
    "VAR x = 0" [:= "x" 0]
    "VAR x = 0 OR VAR y = 1" [:or [:= "x" 0] [:= "y" 1]]
    "VAR x = 0 OR VAR y = 1 OR VAR z = 2" [:or [:or [:= "x" 0] [:= "y" 1]] [:= "z" 2]]
    "(VAR x = 0)" [:= "x" 0]
    "(VAR x = 0 OR VAR y = 1)" [:or [:= "x" 0] [:= "y" 1]]
    "VAR \"x:\" = 0" [:= "x:" 0]
    "(VAR \"x$\" = 0 OR VAR y = 1)" [:or [:= "x$" 0] [:= "y" 1]]))

(deftest plan-density-event
  (are [s sexpr] (= sexpr (plan s :start :density-event))
    "VAR x = 0" {"x" 0}
    "(VAR x = 0)" {"x" 0}
    "VAR x = 0 AND VAR y = 1" {"x" 0 "y" 1}
    "(VAR x = 0) AND VAR y = 1" {"x" 0 "y" 1}
    "VAR x = 0 AND (VAR y = 1)" {"x" 0 "y" 1}
    "(VAR x = 0 AND VAR y = 1)" {"x" 0 "y" 1}
    "VAR x = 0 AND VAR y = 1 AND VAR z = 2" {"x" 0 "y" 1 "z" 2}
    "(VAR x = 0) AND VAR y = 1 AND VAR z = 2" {"x" 0 "y" 1 "z" 2}
    "VAR x = 0 AND (VAR y = 1) AND VAR z = 2" {"x" 0 "y" 1 "z" 2}
    "VAR x = 0 AND VAR y = 1 AND (VAR z = 2)" {"x" 0 "y" 1 "z" 2}
    "(VAR x = 0 AND VAR y = 1) AND VAR z = 2" {"x" 0 "y" 1 "z" 2}
    "VAR x = 0 AND (VAR y = 1 AND VAR z = 2)" {"x" 0 "y" 1 "z" 2}
    "(VAR x = 0 AND VAR y = 1 AND VAR z = 2)" {"x" 0 "y" 1 "z" 2}
    "(VAR \"|x\" = 0 AND VAR y = 1 AND VAR z = 2)" {"|x" 0 "y" 1 "z" 2}))

(defn eval
  [expr env & tuples]
  (apply scalar/eval (plan expr) env {} tuples))

(deftest eval-symbol-env
  (are [s env expected] (= expected
                           (try (eval s env)
                                (catch #?(:clj Exception :cljs :default) _
                                  :error)))
    "x" {"x" 0} 0
    "x" {} :error))

(deftest eval-symbol-tuple
  (are [expected s env m attrs name] (let [tuple (tuple/tuple m :name name :attrs attrs)]
                                       (= expected (eval s env tuple)))
    nil "x" {} {} ["x"] "data"
    nil "data.x" {} {} ["x"] "data"
    0 "x" {"x" 0} {} ["x"] "data"
    0 "x" {} {"x" 0} ["x"] "data"
    0 "data.x" {} {"x" 0} ["x"] "data"))

(deftest eval-operator-no-env
  (are [expected s] (= expected (eval s {}))
    false "NOT true"
    true "NOT false"

    true "0 IS 0"
    false "0 IS 1"
    false "1 IS 0"

    false "0 IS NOT 0"
    true "0 IS NOT 1"
    true "1 IS NOT 0"

    true "NULL IS NULL"
    false "NULL IS NOT NULL"
    false "0 IS NULL"
    true  "0 IS NOT NULL"))

(deftest eval-operator-env
  (are [expected s env] (= expected
                           (try (eval s env)
                                (catch #?(:clj Exception :cljs :default) _
                                  :error)))
    0 "x" {"x" 0}
    :error "x" {}

    true "x IS 0" {"x" 0}
    false "x IS 0" {"x" 1}
    true "0 IS x" {"x" 0}
    false "0 IS x" {"x" 1}

    false "x IS NOT 0" {"x" 0}
    true "x IS NOT 0" {"x" 1}
    false "0 IS NOT x" {"x" 0}
    true  "0 IS NOT x" {"x" 1}

    :error "x IS NULL" {}
    false "x IS NOT NULL" {"x" nil}
    false "x IS NOT NULL" {"x" nil}
    false "x IS NULL" {"x" 0}
    true "x IS NOT NULL" {"x" 0}

    true "\"x \" IS NOT NULL" {"x " 0}))

(deftest eval-operator-tuple
  (are [expected s env m attrs] (= expected
                                   (let [tuple (tuple/tuple m :attrs attrs)]
                                     (try (eval s env tuple)
                                          (catch #?(:clj Exception :cljs :default) _
                                            :error))))
    true "x IS NULL" {"x" nil} {} ["x"]
    true "x IS NULL" {} {"x" nil} ["x"]

    1   "x + 1" {} {"x" 0} ["x"]
    nil "x + 1" {} {}      ["x"]

    nil "\"|x\" + 1" {} {} ["|x"]))

(deftest eval-mutual-info
  (let [model (reify gpm.proto/MutualInfo
                (mutual-info [_ _ _]
                  7))
        env {"model" model}]
    (is (= 7 (eval "MUTUAL INFORMATION OF VAR x > 0 WITH VAR x < 0 UNDER model" env)))
    (is (= 7 (eval "MUTUAL INFORMATION OF VAR \"#x\" > 0 WITH VAR \"#x\" < 0 UNDER model" env)))))

(deftest eval-approximate-mutual-info
  (let [simulate-count (atom 0)
        model (reify gpm.proto/GPM
                (simulate [_this _targets _constraints]
                  (swap! simulate-count inc)
                  {})

                (logpdf [_this _targets _constraints]
                  0))
        env {"model" model}]
    (eval "APPROXIMATE MUTUAL INFORMATION OF VAR x WITH VAR y UNDER model" env)
    (is (= 1000 @simulate-count))))

(deftest eval-transforms
  (are [s env expected] (= expected
                           (try (eval s env)
                                (catch #?(:clj Exception :cljs :default) e
                                  :error)))
    "log(x)" {"x" 1} 0.0
    "log(\"#x\")" {"#x" 1} 0.0
    "log(1)" {} 0.0
    "log(0.5)" {} -0.6931471805599453 ; spot check
    "log(x)" {"x" 0.8} -0.2231435513142097 ; spot check
    "log(x) - log(y)" {"x" 0.5 "y" 0.2} 0.916290731874155; spot check
    "log(x)" {} :error))

(deftest condition
  (let [model (reify gpm.proto/Condition
                (condition [gpm conditions]
                  (conditioned/condition gpm conditions)))
        conditioned? #(instance? ConditionedGPM %)]
    (is (conditioned? (scalar/condition model {"x" 0})))
    (is (not (conditioned? (scalar/condition model {}))))
    (is (not (conditioned? (scalar/condition model {"x" nil}))))))

(deftest constrain
  (let [model (reify gpm.proto/Constrain
                (constrain [gpm event opts]
                  (constrained/constrain gpm event opts)))
        constrained? #(instance? ConstrainedGPM %)]
    (is (constrained? (scalar/constrain model '(> x 0))))
    (is (not (constrained? (scalar/constrain model nil))))
    (is (not (constrained? (scalar/constrain model '(> x nil)))))))
