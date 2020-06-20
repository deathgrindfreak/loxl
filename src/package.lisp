(defpackage :token-type
  (:use :common-lisp)
  (:export
   :is-keyword
   :+keywords+
   :+token-type+))

(defpackage :token
  (:use :common-lisp :token-type)
  (:export
   :token))

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
