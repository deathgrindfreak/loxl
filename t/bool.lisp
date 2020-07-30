(in-package :loxl-tests)

(def-suite bool-tests
  :description "Tests involving booleans (true/false)"
  :in all-tests)

(in-suite bool-tests)

(test-loxl-file "bool/equality"
  :expected '("true" "nil" "nil" "true" "nil" "nil" "nil" "nil" "nil" "nil"
              "true" "true" "nil" "true" "true" "true" "true" "true"))

(test-loxl-file "bool/not"
  :expected '("nil" "true" "true"))
