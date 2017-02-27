(defmodule unit-rule-engine-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from ltest
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))
    (from rule-engine
      (check 1)
      (evaluate 1)
      (evaluate 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest rule-engine-check-good-simple
  (is-equal (check 'true) 'predicate)
  (is-equal (check '()) 'null)
  (is-equal (check "") 'null)
  (is-equal (check 'asd) 'atom)
  (is-equal (check '(first (1 2 3))) 'integer)
  (is-equal (check '(hello "asd" 3)) 'list)
  (is-equal (check '(1 2 3)) 'list)
  (is-equal (check "asd") 'string)
  (is-equal (check '(or true false)) 'predicate)
  (is-equal (check '(eq 1 2)) 'predicate)
  (is-equal (check '(in "asd" ("zxc" "asd"))) 'predicate))

(deftest rule-engine-check-bad-simple
  (is-not (check '(or true 1)))
  (is-not (check '(in true or))))

(deftest rule-engine-evaluate-logical
  (is (evaluate '(not false)))
  (is (evaluate '(and true)))
  (is (evaluate '(or true)))
  (is (evaluate '(or true false)))
  (is-not (evaluate '(not true)))
  (is-not (evaluate '(and false)))
  (is-not (evaluate '(or false)))
  (is-not (evaluate '(and true false))))

(deftest rule-engine-evaluate-equations
  (is (evaluate '(eq 0 0)))
  (is (evaluate '(eq "asd" "asd")))
  (is (evaluate '(lt 0 1 2)))
  (is (evaluate '(lte 0 0 1)))
  (is-not (evaluate '(eq 0 0 1)))
  (is-not (evaluate '(eq "asd" "asd" true)))
  (is-not (evaluate '(gt 0 1 2)))
  (is-not (evaluate '(lte 0 0 -1))))

(deftest rule-engine-evaluate-lists
  (is (evaluate '(eq (count (1)) 1)))
  (is (evaluate '(eq (count (1 2)) 2))))


(deftestskip rule-engine-failures
  (is (evaluate '(eq (count ()) 0))))
