(in-package :token)

(defclass token ()
  ((type :initarg :type
         :reader token-type)
   (lexeme :initarg :lexeme
           :reader lexeme)
   (literal :initarg :literal
            :reader literal)
   (line :initarg :line
         :reader token-line)))

(defmethod print-object ((tk token) out)
  (with-slots (type lexeme literal) tk
    (print-unreadable-object (tk out :type t)
      (format out "~a ~a ~a" type lexeme literal))))
