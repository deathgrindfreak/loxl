(in-package :loxl-tests)

(def-suite call-tests
  :description "Tests involving function calls"
  :in all-tests)

(in-suite call-tests)

(test-loxl-file "call/bool"
  :expected '("[line 1] Error: Can only call functions and classes."))

(test-loxl-file "call/nil"
  :expected '("[line 1] Error: Can only call functions and classes."))

(test-loxl-file "call/num"
  :expected '("[line 1] Error: Can only call functions and classes."))

(test-loxl-file "call/object"
  :skip "Need to implement objects first"
  :expected '())

(test-loxl-file "call/string"
  :expected '("[line 1] Error: Can only call functions and classes."))
