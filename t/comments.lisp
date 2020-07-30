(in-package :loxl-tests)

(def-suite comments-tests
  :description "Tests involving comments"
  :in all-tests)

(in-suite comments-tests)

(test-loxl-file "comments/line_at_eof"
  :expected '("ok"))

(test-loxl-file "comments/only_line_comment"
  :expected '())

(test-loxl-file "comments/only_line_comment_and_line"
  :expected '())

(test-loxl-file "comments/unicode"
  :expected '("ok"))
