(in-package :loxl-tests)

(def-suite string-tests
  :description "Tests involving strings"
  :in all-tests)

(in-suite string-tests)

(test-loxl-file "string/error_after_multiline" :expected '("[line 7] Error: Undefined variable 'err'."))
(test-loxl-file "string/literals" :expected '("()" "a string" "A~¶Þॐஃ"))
(test-loxl-file "string/multiline" :expected '("1" "2" "3"))
(test-loxl-file "string/unterminated" :expected '("[line 2] Error: Unterminated string."))
