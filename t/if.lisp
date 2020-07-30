(in-package :loxl-tests)

(def-suite if-tests
  :description "Tests involving conditionals"
  :in all-tests)

(in-suite if-tests)

(test-loxl-file "if/class_in_else"
  :skip "Need to implement classes"
  :expected '())

(test-loxl-file "if/class_in_then"
  :skip "Need to implement classes"
  :expected '())

(test-loxl-file "if/dangling_else"
  :expected '("good"))

(test-loxl-file "if/else"
  :expected '("good" "good" "block"))

(test-loxl-file "if/fun_in_else"
  :expected '("[line 2] Error at 'fun': Expect expression."))

(test-loxl-file "if/fun_in_then"
  :expected '("[line 2] Error at 'fun': Expect expression."))

(test-loxl-file "if/if"
  :expected '("good" "block" "true"))

(test-loxl-file "if/truth"
  :expected '("false" "nil" "true" "0" "empty"))

(test-loxl-file "if/var_in_else"
  :expected '("[line 2] Error at 'var': Expect expression."))

(test-loxl-file "if/var_in_then"
  :expected '("[line 2] Error at 'var': Expect expression."))
