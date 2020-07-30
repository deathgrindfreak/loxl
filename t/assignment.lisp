(in-package :loxl-tests)

(def-suite assignment-tests
  :description "Test involving the assignment operator (=)"
  :in all-tests)

(in-suite assignment-tests)

(test-loxl-file "assignment/associativity"
  :expected '("c" "c" "c"))

(test-loxl-file "assignment/global"
  :expected '("before" "after" "arg" "arg"))

(test-loxl-file "assignment/grouping"
  :expected '("[line 2] Error at '=': Invalid assignment target."))

(test-loxl-file "assignment/infix_operator"
  :expected '("[line 3] Error at '=': Invalid assignment target."))

(test-loxl-file "assignment/local"
  :expected '("before" "after" "arg" "arg"))

(test-loxl-file "assignment/prefix_operator"
  :expected '("[line 2] Error at '=': Invalid assignment target."))

(test-loxl-file "assignment/syntax"
  :expected '("var" "var"))

(test-loxl-file "assignment/to_this"
  :skip "Need to implement classes first"
  :expected '())

(test-loxl-file "assignment/undefined"
  :expected '("[line 1] Error: Undefined variable 'unknown'."))
