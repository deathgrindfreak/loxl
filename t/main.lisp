(in-package :loxl-tests)

(def-suite all-tests
  :description "LOXL interpreter tests")

(in-suite all-tests)

(defun test-loxl ()
  (run! 'all-tests))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun format-loxl-output (in-str)
    (with-input-from-string (in in-str)
      (loop for line = (read-line in nil nil)
            while line
            collect line))))

(defmacro test-loxl-file (file &body body)
  (let ((out-str (gensym))
        (out (gensym))
        (expected-output (getf body :expected))
        (skip-test (getf body :skip)))
    `(test ,(intern file)
       ,(if skip-test
            `(skip ,skip-test)
            `(let ((,out-str (make-array '(0)
                                         :element-type 'base-char
                                         :adjustable t
                                         :fill-pointer 0)))
               (with-output-to-string (,out ,out-str)
                 (let ((*standard-output* ,out))
                   (loxl:main ',(list (concatenate 'string "data/" file ".lox"))))
                 (is (equal (format-loxl-output ,out-str) ,expected-output))))))))

(def-suite assignment-tests
  :description "Test involving the assignment operator (=)"
  :in all-tests)

(in-suite assignment-tests)

(test-loxl-file "assignment/associativity"
  :expected '("c" "c" "c"))

(test-loxl-file "assignment/global"
  :expected '("before" "after" "arg" "arg"))

(test-loxl-file "assignment/grouping"
  :expected '("[line 2] Error at '=': Invalid assignment target."))

(test-loxl-file "assignment/infix_operator"
  :expected '("[line 3] Error at '=': Invalid assignment target."))

(test-loxl-file "assignment/local"
  :expected '("before" "after" "arg" "arg"))

(test-loxl-file "assignment/prefix_operator"
  :expected '("[line 2] Error at '=': Invalid assignment target."))

(test-loxl-file "assignment/syntax"
  :expected '("var" "var"))

(test-loxl-file "assignment/to_this"
  :skip "Need to implement classes first"
  :expected '())

(test-loxl-file "assignment/undefined"
  :expected '("[line 1] Error: Undefined variable 'unknown'."))

(def-suite block-tests
  :description "Tests involving blocks ({})"
  :in all-tests)

(in-suite block-tests)

(test-loxl-file "block/empty"
  :expected '("ok"))

(test-loxl-file "block/scope"
  :expected '("inner" "outer"))

(def-suite bool-tests
  :description "Tests involving booleans (true/false)"
  :in all-tests)

(in-suite bool-tests)

(test-loxl-file "bool/equality"
  :expected '("T" "nil" "nil" "T" "nil" "nil" "nil" "nil" "nil" "nil"
              "T" "T" "nil" "T" "T" "T" "T" "T"))

(test-loxl-file "bool/not"
  :expected '("nil" "T" "T"))

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

(def-suite for-tests
  :description "Tests involving for loops"
  :in all-tests)

(in-suite for-tests)

(test-loxl-file "for/class_in_body"
  :skip "Need to implement classes"
  :expected '())

(test-loxl-file "for/closure_in_body"
  :expected '("4" "1" "4" "2" "4" "3"))

(test-loxl-file "for/fun_in_body"
  :expected '("[line 2] Error at 'fun': Expect expression."))

(test-loxl-file "for/return_closure"
  :expected '("i"))

(test-loxl-file "for/return_inside"
  :expected '("i"))

(test-loxl-file "for/scope"
  :expected '("0" "-1" "after" "0"))

(test-loxl-file "for/statement_condition"
  :expected '("[line 3] Error at '{': Expect expression."
              "[line 3] Error at ')': Expect ';' after expression."))

(test-loxl-file "for/statement_increment"
  :expected '("[line 2] Error at '{': Expect expression."))

(test-loxl-file "for/statement_initializer"
  :expected '("[line 3] Error at '{': Expect expression."
              "[line 3] Error at ')': Expect ';' after expression."))

(test-loxl-file "for/syntax"
  :expected '("1" "2" "3" "0" "1" "2" "done" "0" "1" "0" "1" "2" "0" "1"))

(test-loxl-file "for/var_in_body"
  :expected '("[line 2] Error at 'var': Expect expression."))

(def-suite function-tests
  :description "Tests involving functions"
  :in all-tests)

(in-suite function-tests)

(test-loxl-file "function/body_must_be_block"
  :expected '())

(test-loxl-file "function/empty_body"
  :expected '("nil"))

(test-loxl-file "function/extra_arguments"
  :expected '("[line 6] Error: Expected 2 arguments but got 4."))

(test-loxl-file "function/local_mutual_recursion"
  :expected '("21"))

(test-loxl-file "function/local_recursion"
  :expected '("21"))

(test-loxl-file "function/missing_arguments"
  :expected '("[line 3] Error: Expected 2 arguments but got 1."))

(test-loxl-file "function/missing_comma_in_parameters" :expected '())

(test-loxl-file "function/mutual_recursion"
  :expected '("T" "T"))

(test-loxl-file "function/parameters"
  :expected '("0" "1" "3" "6" "10" "15" "21" "28" "36"))

(test-loxl-file "function/print"
  :expected '("<fn foo>" "<native fn>"))

(test-loxl-file "function/recursion"
  :expected '("21"))

(test-loxl-file "function/too_many_arguments"
  :expected '("[line 260] Error at 'a': Cannot have more than 255 arguments."))

(test-loxl-file "function/too_many_parameters"
  :expected '("[line 257] Error at 'a': Cannot have more than 255 parameters."))
