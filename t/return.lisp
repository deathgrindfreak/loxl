(in-package :loxl-tests)

(def-suite return-tests
  :description "Tests involving returning from functions"
  :in all-tests)

(in-suite return-tests)

(test-loxl-file "return/after_else" :expected '("ok"))
(test-loxl-file "return/after_if" :expected '("ok"))
(test-loxl-file "return/after_while" :expected '("ok"))
(test-loxl-file "return/at_top_level" :expected '("[line 1] Error at 'return': Cannot return from top-level code."))
(test-loxl-file "return/in_function" :expected '("ok"))
(test-loxl-file "return/in_method" :skip "Implement methods" :expected '())
(test-loxl-file "return/return_nil_if_no_value" :expected '("nil"))
