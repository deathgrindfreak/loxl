(in-package :loxl-tests)

(def-suite function-tests
  :description "Tests involving functions"
  :in all-tests)

(in-suite function-tests)

(test-loxl-file "function/body_must_be_block"
  :expected '())

(test-loxl-file "function/empty_body"
  :expected '("nil"))

(test-loxl-file "function/extra_arguments"
  :expected '("[line 6] Error: Expected 2 arguments but got 4."))

(test-loxl-file "function/local_mutual_recursion"
  :expected '("21"))

(test-loxl-file "function/local_recursion"
  :expected '("21"))

(test-loxl-file "function/missing_arguments"
  :expected '("[line 3] Error: Expected 2 arguments but got 1."))

(test-loxl-file "function/missing_comma_in_parameters" :expected '())

(test-loxl-file "function/mutual_recursion"
  :expected '("T" "T"))

(test-loxl-file "function/parameters"
  :expected '("0" "1" "3" "6" "10" "15" "21" "28" "36"))

(test-loxl-file "function/print"
  :expected '("<fn foo>" "<native fn>"))

(test-loxl-file "function/recursion"
  :expected '("21"))

(test-loxl-file "function/too_many_arguments"
  :expected '("[line 260] Error at 'a': Cannot have more than 255 arguments."))

(test-loxl-file "function/too_many_parameters"
  :expected '("[line 257] Error at 'a': Cannot have more than 255 parameters."))
