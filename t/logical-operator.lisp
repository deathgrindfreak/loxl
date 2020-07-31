(in-package :loxl-tests)

(def-suite logical-operator-tests
  :description "Tests involving logical operators"
  :in all-tests)

(in-suite logical-operator-tests)

(test-loxl-file "logical_operator/and"
  :expected '("nil" "1" "nil" "true" "3" "true" "nil"))

(test-loxl-file "logical_operator/and_truth"
  :expected '("nil" "nil" "ok" "ok" "ok"))

(test-loxl-file "logical_operator/or"
  :expected '("1" "1" "true" "nil" "nil" "nil" "true"))

(test-loxl-file "logical_operator/or_truth"
  :expected '("ok" "ok" "true" "0" "s"))
