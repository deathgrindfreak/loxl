(in-package :loxl-tests)

(def-suite nil-tests
  :description "Tests involving nil"
  :in all-tests)

(in-suite nil-tests)

(test-loxl-file "nil/literal"
  :expected '("nil"))
