;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :loxl
  :name "loxl"
  :version "0.0.0"
  :author "Cooper Bell"
  :license "BSD"
  :description "loxl"
  :long-description "Lox Language Interpreter"
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "token-type")
               (:file "token")
               (:file "scanner")
               (:file "ast")
               (:file "parser")
               (:file "runtime-error")
               (:file "environment")
               (:file "interpreter")
               (:file "lox-callable")
               (:file "loxl"))
  :in-order-to ((test-op (test-op "loxl/tests"))))

(defsystem :loxl/tests
  :depends-on (:loxl :fiveam)
  :components ((:module "t"
                :serial t
                :components ((:file "package")
                             (:file "main"))))
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run! (uiop:find-symbol* 'all-tests :loxl-tests))))
