(in-package :loxl-tests)

(def-suite operator-tests
  :description "Tests involving operators"
  :in all-tests)

(in-suite operator-tests)

(test-loxl-file "operator/add" :expected '("579" "string"))
(test-loxl-file "operator/add_bool_nil" :expected '("[line 1] Error: Operands must be two numbers or two strings."))
(test-loxl-file "operator/add_bool_num" :expected '("[line 1] Error: Operands must be two numbers or two strings."))
(test-loxl-file "operator/add_bool_string" :expected '("[line 1] Error: Operands must be two numbers or two strings."))
(test-loxl-file "operator/add_nil_nil" :expected '("[line 1] Error: Operands must be two numbers or two strings."))
(test-loxl-file "operator/add_num_nil" :expected '("[line 1] Error: Operands must be two numbers or two strings."))
(test-loxl-file "operator/add_string_nil" :expected '("[line 1] Error: Operands must be two numbers or two strings."))
(test-loxl-file "operator/comparison"
  :expected '("true" "nil" "nil" "true" "true" "nil" "nil" "nil" "true" "nil"
              "true" "true" "nil" "nil" "nil" "nil" "true" "true" "true" "true"))
(test-loxl-file "operator/divide" :expected '("4" "1.0"))
(test-loxl-file "operator/divide_nonnum_num" :expected '("[line 1] Error: Operands must be numbers."))
(test-loxl-file "operator/divide_num_nonnum" :expected '("[line 1] Error: Operands must be numbers."))
(test-loxl-file "operator/equals" :expected '("true" "true" "nil" "true" "nil" "true" "nil" "true" "nil" "nil"))
(test-loxl-file "operator/equals_class" :skip "Need to implement classes" :expected '())
(test-loxl-file "operator/equals_method" :skip "Need to implement methods" :expected '())
(test-loxl-file "operator/greater_nonnum_num" :expected '("[line 1] Error: Operands must be numbers."))
(test-loxl-file "operator/greater_num_nonnum" :expected '("[line 1] Error: Operands must be numbers."))
(test-loxl-file "operator/greater_or_equal_nonnum_num" :expected '("[line 1] Error: Operands must be numbers."))
(test-loxl-file "operator/greater_or_equal_num_nonnum" :expected '("[line 1] Error: Operands must be numbers."))
(test-loxl-file "operator/less_nonnum_num" :expected '("[line 1] Error: Operands must be numbers."))
(test-loxl-file "operator/less_num_nonnum" :expected '("[line 1] Error: Operands must be numbers."))
(test-loxl-file "operator/less_or_equal_nonnum_num" :expected '("[line 1] Error: Operands must be numbers."))
(test-loxl-file "operator/less_or_equal_num_nonnum" :expected '("[line 1] Error: Operands must be numbers."))
(test-loxl-file "operator/multiply" :expected '("15" "3.7020001"))
(test-loxl-file "operator/multiply_nonnum_num" :expected '("[line 1] Error: Operands must be numbers."))
(test-loxl-file "operator/multiply_num_nonnum" :expected '("[line 1] Error: Operands must be numbers."))
(test-loxl-file "operator/negate" :expected '("-3" "3" "-3"))
(test-loxl-file "operator/negate_nonnum" :expected '("[line 1] Error: Operands must be numbers."))
(test-loxl-file "operator/not" :expected '("nil" "true" "true" "nil" "nil" "true" "nil" "nil"))
(test-loxl-file "operator/not_class" :skip "Implement classes" :expected '())
(test-loxl-file "operator/not_equals" :expected '("nil" "nil" "true" "nil" "true" "nil" "true" "nil" "true" "true"))
(test-loxl-file "operator/subtract" :expected '("1" "0.0"))
(test-loxl-file "operator/subtract_nonnum_num" :expected '("[line 1] Error: Operands must be numbers."))
(test-loxl-file "operator/subtract_num_nonnum" :expected '("[line 1] Error: Operands must be numbers."))
