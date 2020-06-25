(defpackage :util
  (:use
   :common-lisp)
  (:export
   :with-gensyms
   :as-keyword))

(defpackage :token-type
  (:use
   :common-lisp
   :util)
  (:export
   :is-keyword
   :*keywords*
   :*token-type*))

(defpackage :token
  (:use
   :common-lisp)
  (:export
   :token
   :token-type
   :lexeme
   :literal
   :token-line))

(defpackage :ast
  (:use
   :common-lisp
   :util
   :token)
  (:export
   :expr-stmt
   :print-stmt
   :expr
   :ternary
   :binary
   :grouping
   :literal
   :unary
   :print-ast))

(defpackage :scanner
  (:use
   :common-lisp
   :token-type
   :token)
  (:export
   :scanner
   :scanner-error
   :error-line
   :scanner-error-message
   :message
   :scan-tokens))

(defpackage :parser
  (:use
   :common-lisp
   :util
   :ast
   :token-type
   :token)
  (:export
   :parse
   :parser
   :parser-error
   :error-token
   :parser-error-message))

(defpackage :interpreter
  (:use
   :common-lisp
   :token
   :ast)
  (:export
   :interpret
   :runtime-error
   :runtime-error-token
   :runtime-error-message))

(defpackage :loxl
  (:use
   :common-lisp
   :scanner
   :parser
   :interpreter
   :token
   :ast)
  (:export
   :main))
