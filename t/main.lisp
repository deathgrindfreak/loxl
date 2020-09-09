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
                                         :element-type 'character
                                         :adjustable t
                                         :fill-pointer 0)))
               (with-output-to-string (,out ,out-str)
                 (let ((*standard-output* ,out))
                   (loxl:main ',(list (concatenate 'string "../t/data/" file ".lox"))))
                 (is (equal (format-loxl-output ,out-str) ,expected-output))))))))
