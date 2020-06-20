(in-package :token)

(defclass token ()
  ((type :initarg :type)
   (lexeme :initarg :lexeme)
   (literal :initarg :literal)
   (line :initarg :line)))

(defmethod initialize-instance :after ((tk token) &key type)
  ;; Ensure type is a proper token type
  (unless (member type +token-type+)
    (error (format nil "Bad token type: ~s" type))))

(defmethod print-object ((tk token) out)
  (with-slots (type lexeme literal) tk
    (print-unreadable-object (tk out :type t)
      (format out "~a ~a ~a" type lexeme literal))))
