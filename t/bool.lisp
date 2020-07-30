(in-package :loxl-tests)

(def-suite bool-tests
  :description "Tests involving booleans (true/false)"
  :in all-tests)

(in-suite bool-tests)

(test-loxl-file "bool/equality"
  :expected '("T" "nil" "nil" "T" "nil" "nil" "nil" "nil" "nil" "nil"
              "T" "T" "nil" "T" "T" "T" "T" "T"))

(test-loxl-file "bool/not"
  :expected '("nil" "T" "T"))
