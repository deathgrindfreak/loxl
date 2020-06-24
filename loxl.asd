;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem loxl
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
               (:file "interpreter")
               (:file "loxl")))
