(in-package :token)

(defclass token ()
  ((type :initarg :type)
   (lexeme :initarg :lexeme)
   (literal :initarg :literal)
   (line :initarg :line)))

(defmethod print-object ((tk token) out)
  (with-slots (type lexeme literal) tk
    (print-unreadable-object (tk out :type t)
      (format out "~a ~a ~a" type lexeme literal))))
