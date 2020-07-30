(in-package :loxl-tests)

(def-suite closure-tests
  :description "Tests involving closures"
  :in all-tests)

(in-suite closure-tests)

(test-loxl-file "closure/assign_to_closure"
  :expected '("local" "after f" "after f" "after g"))

(test-loxl-file "closure/assign_to_shadowed_later"
  :skip "Closures still need work"
  :expected '("inner" "assigned"))

(test-loxl-file "closure/close_over_function_parameter"
  :expected '("param"))

(test-loxl-file "closure/close_over_later_variable"
  :expected '("b" "a"))

(test-loxl-file "closure/close_over_method_parameter"
  :skip "Need to implement classes"
  :expected '())

(test-loxl-file "closure/closed_closure_in_function"
  :expected '("local"))

(test-loxl-file "closure/nested_closure"
  :expected '("a" "b" "c"))

(test-loxl-file "closure/open_closure_in_function"
  :expected '("local"))

(test-loxl-file "closure/reference_closure_multiple_times"
  :expected '("a" "a"))

(test-loxl-file "closure/reuse_closure_slot"
  :expected '("a"))

(test-loxl-file "closure/shadow_closure_with_local"
  :expected '("closure" "shadow" "closure"))

(test-loxl-file "closure/unused_closure"
  :expected '("ok"))

(test-loxl-file "closure/unused_later_closure"
  :expected '("a"))
