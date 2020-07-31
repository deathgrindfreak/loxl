(in-package :loxl-tests)

(def-suite number-tests
  :description "Tests involving numbers"
  :in all-tests)

(in-suite number-tests)

(test-loxl-file "number/decimal_point_at_eof"
  :skip "Methods need to be implemented"
  :expected '())

(test-loxl-file "number/leading_dot"
  :expected '("[line 2] Error at '.': Expect expression."))

(test-loxl-file "number/literals"
  :expected '("123" "987654" "0" "0" "123.456" "-0.001"))

(test-loxl-file "number/nan_equality"
  :skip "Nan is not implemented"
  :expected '())

(test-loxl-file "number/trailing_dot"
  :skip "Methods need to be implemented"
  :expected '())
