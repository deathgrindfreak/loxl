(in-package :loxl-tests)

(def-suite print-tests
  :description "Tests involving print function"
  :in all-tests)

(in-suite print-tests)

(test-loxl-file "print/missing_argument" :expected '("[line 2] Error at ';': Expect expression."))
