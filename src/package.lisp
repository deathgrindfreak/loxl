(defpackage :token-type
  (:use :common-lisp)
  (:export
   :+token-type+))

(defpackage :token
  (:use :common-lisp :token-type)
  (:export
   :token))

(defpackage :scanner
  (:use :common-lisp :token-type :token))

(defpackage :loxl
  (:use :common-lisp)
  (:export
   :main))
