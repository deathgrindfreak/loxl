(in-package :loxl-tests)

(def-suite block-tests
  :description "Tests involving blocks ({})"
  :in all-tests)

(in-suite block-tests)

(test-loxl-file "block/empty"
  :expected '("ok"))

(test-loxl-file "block/scope"
  :expected '("inner" "outer"))
