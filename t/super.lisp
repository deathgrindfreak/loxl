(in-package :loxl-tests)

(def-suite super-tests
  :description "Tests involving super"
  :in all-tests)

(in-suite super-tests)

(test-loxl-file "super/bound_method" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/call_other_method" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/call_same_method" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/closure" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/constructor" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/extra_arguments" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/indirectly_inherited" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/missing_arguments" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/no_superclass_bind" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/no_superclass_call" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/no_superclass_method" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/parenthesized" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/reassign_superclass" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/super_at_top_level" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/super_in_closure_in_inherited_method" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/super_in_inherited_method" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/super_in_top_level_function" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/super_without_dot" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/super_without_name" :skip "Need to implement classes." :expected '())
(test-loxl-file "super/this_in_superclass_method" :skip "Need to implement classes." :expected '())
