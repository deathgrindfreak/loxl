(in-package :loxl-tests)

(def-suite this-tests
  :description "Tests involving this"
  :in all-tests)

(in-suite this-tests)

(test-loxl-file "this/closure" :skip "Need to implement classes." :expected '())
(test-loxl-file "this/nested_class" :skip "Need to implement classes." :expected '())
(test-loxl-file "this/nested_closure" :skip "Need to implement classes." :expected '())
(test-loxl-file "this/this_at_top_level" :skip "Need to implement classes." :expected '())
(test-loxl-file "this/this_in_method" :skip "Need to implement classes." :expected '())
(test-loxl-file "this/this_in_top_level_function" :skip "Need to implement classes." :expected '())
