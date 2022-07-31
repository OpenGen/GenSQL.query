(ns inferenceql.query.scalar-test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer [are deftest is testing]]
            [inferenceql.inference.gpm.proto :as gpm.proto]
            [inferenceql.query.strict.parser :as parser]
            [inferenceql.query.scalar :as scalar]
            [inferenceql.query.tuple :as tuple]))

(defn plan
  [s]
  (-> (parser/parse s :start :scalar-expr)
      (scalar/plan)))

(deftest plan-symbol
  (is (= 'x (plan "x"))))

(deftest plan-operator
  (are [s sexpr] (= sexpr (plan s))
    "x or y" '(or x y)
    "x and y" '(and x y)
    "not x" '(not x)
    "x > y" '(> x y)
    "x >= y" '(>= x y)
    "x = 0" '(= x 0)
    "x <= y" '(<= x y)
    "x < y" '(< x y)
    "x + y" '(+ x y)
    "x - y" '(- x y)
    "x * y" '(* x y)
    "x / y" '(/ x y)))

(deftest plan-operator-precedence-same
  (are [s sexpr] (= sexpr (plan s))
    "x or y or z" '(or (or x y) z)
    "x and y and z" '(and (and x y) z)
    "x + y + z" '(+ (+ x y) z)
    "x - y - z" '(- (- x y) z)
    "x * y * z" '(* (* x y) z)
    "x / y / z" '(/ (/ x y) z)
    "x > y > z" '(> (> x y) z)
    "x >= y >= z" '(>= (>= x y) z)
    "x = y = z" '(= (= x y) z)
    "x <= y <= z" '(<= (<= x y) z)
    "x < y < z" '(< (< x y) z)
    "x is y is z" '(= (= x y) z)))

(deftest plan-operator-precedence-different
  (are [s sexpr] (= sexpr (plan s))
    "x and y or z" '(or (and x y) z)
    "x or y and z" '(or x (and y z))
    "not x and y" '(and (not x) y)
    "not x or y" '(or (not x) y)
    "not x = y" '(not (= x y))
    "x = y + z" '(= x (+ y z))
    "x + y = z" '(= (+ x y) z)
    "x + y * z" '(+ x (* y z))
    "x * y + z" '(+ (* x y) z)))

(deftest plan-operator-group
  (are [s sexpr] (= sexpr (plan s))
    "x and (y or z)" '(and x (or y z))
    "(x or y) and z" '(and (or x y) z)
    "(not x) and y" '(and (not x) y)
    "(not x) or y" '(or (not x) y)
    "(not x) = y" '(= (not x) y)
    "(x = y) + z" '(+ (= x y) z)
    "x + (y = z)" '(+ x (= y z))
    "(x + y) * z" '(* (+ x y) z)
    "x * (y + z)" '(* x (+ y z))))

(defn eval
  [expr env & tuples]
  (apply scalar/eval (plan expr) env {} tuples))

(deftest eval-symbol-env
  (are [s env expected] (= expected
                           (try (eval s env)
                                (catch #?(:clj Exception :cljs :default) e
                                  :error)))
    "x" '{x 0} 0
    "x" '{} :error))

(deftest eval-symbol-tuple
  (are [expected s env m attrs name] (let [tuple (tuple/tuple m :name name :attrs attrs)]
                                       (= expected (eval s env tuple)))
    nil "x" '{} '{} '[x] 'data
    nil "data.x" '{} '{} '[x] 'data
    0 "x" '{x 0} '{} '[x] 'data
    0 "x" '{} '{x 0} '[x] 'data
    0 "data.x" '{} '{x 0} '[x] 'data))

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
    0 "x" '{x 0}
    :error "x" '{}

    true "x IS 0" '{x 0}
    false "x IS 0" '{x 1}
    true "0 IS x" '{x 0}
    false "0 IS x" '{x 1}

    false "x IS NOT 0" '{x 0}
    true "x IS NOT 0" '{x 1}
    false "0 IS NOT x" '{x 0}
    true    "0 IS NOT x"    '{x 1}

    :error "x IS NULL" '{}
    false "x IS NOT NULL" '{x nil}
    false "x IS NOT NULL" '{x nil}
    false "x IS NULL" '{x 0}
    true "x IS NOT NULL" '{x 0}))

(deftest eval-operator-tuple
  (are [expected s env m attrs] (= expected
                                   (let [tuple (tuple/tuple m :attrs attrs)]
                                     (try (eval s env tuple)
                                          (catch #?(:clj Exception :cljs :default) _
                                            :error))))
    true "x IS NULL" '{x nil} '{} '[x]
    true "x IS NULL" '{} '{x nil} '[x]

    1 "x + 1" '{} '{x 0} '[x]
    nil "x + 1" '{} '{} '[x]))

(deftest eval-mutual-info
  (let [model (reify gpm.proto/MutualInfo
                (mutual-info [_ _ _]
                  7))
        env {'model model}]
    (is (= 7 (eval "MUTUAL INFORMATION OF VAR x > 0 WITH VAR x < 0 UNDER model" env)))))

(deftest eval-approximate-mutual-info
  (let [simulate-count (atom 0)
        model (reify gpm.proto/GPM
                (simulate [_this _targets _constraints]
                  (swap! simulate-count inc)
                  {})

                (logpdf [_this _targets _constraints]
                  0))
        env {'model model}]
    (eval "APPROXIMATE MUTUAL INFORMATION OF VAR x WITH VAR y UNDER model" env)
    (is (= 1000 @simulate-count))))
