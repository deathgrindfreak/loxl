(defpackage :util
  (:use :common-lisp)
  (:export
   :as-keyword))

(defpackage :token-type
  (:use :common-lisp :util)
  (:export
   :is-keyword
   :+keywords+
   :+token-type+))

(defpackage :token
  (:use :common-lisp :token-type)
  (:export
   :token
   :token-type
   :lexeme
   :literal
   :token-line))

(defpackage :ast
  (:use :common-lisp :util :token)
  (:export
   :print-ast))

(defpackage :scanner
  (:use :common-lisp :token-type :token)
  (:export
   :scanner
   :scanner-error
   :line
   :message
   :scan-tokens))

(defpackage :loxl
  (:use :common-lisp :scanner)
  (:export
   :main))
