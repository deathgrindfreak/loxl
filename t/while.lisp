(in-package :loxl-tests)

(def-suite while-tests
  :description "Tests involving while loops"
  :in all-tests)

(in-suite while-tests)

(test-loxl-file "while/class_in_body" :skip "Need to implement classes." :expected '())
(test-loxl-file "while/closure_in_body" :expected '("1" "2" "3"))
(test-loxl-file "while/fun_in_body" :expected '("[line 2] Error at 'fun': Expect expression."))
(test-loxl-file "while/return_closure" :expected '("i"))
(test-loxl-file "while/return_inside" :expected '("i"))
(test-loxl-file "while/syntax" :expected '("1" "2" "3" "0" "1" "2"))
(test-loxl-file "while/var_in_body" :expected '("[line 2] Error at 'var': Expect expression."))
