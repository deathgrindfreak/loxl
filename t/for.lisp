(in-package :loxl-tests)

(def-suite for-tests
  :description "Tests involving for loops"
  :in all-tests)

(in-suite for-tests)

(test-loxl-file "for/class_in_body"
  :skip "Need to implement classes"
  :expected '())

(test-loxl-file "for/closure_in_body"
  :expected '("4" "1" "4" "2" "4" "3"))

(test-loxl-file "for/fun_in_body"
  :expected '("[line 2] Error at 'fun': Expect expression."))

(test-loxl-file "for/return_closure"
  :expected '("i"))

(test-loxl-file "for/return_inside"
  :expected '("i"))

(test-loxl-file "for/scope"
  :expected '("0" "-1" "after" "0"))

(test-loxl-file "for/statement_condition"
  :expected '("[line 3] Error at '{': Expect expression."
              "[line 3] Error at ')': Expect ';' after expression."))

(test-loxl-file "for/statement_increment"
  :expected '("[line 2] Error at '{': Expect expression."))

(test-loxl-file "for/statement_initializer"
  :expected '("[line 3] Error at '{': Expect expression."
              "[line 3] Error at ')': Expect ';' after expression."))

(test-loxl-file "for/syntax"
  :expected '("1" "2" "3" "0" "1" "2" "done" "0" "1" "0" "1" "2" "0" "1"))

(test-loxl-file "for/var_in_body"
  :expected '("[line 2] Error at 'var': Expect expression."))
