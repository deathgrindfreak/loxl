(in-package :interpreter)

(defclass environment ()
  ((enclosing :initform nil :initarg :enclosing)
   (values :initform (make-hash-table :test 'equal))))

(defmethod define ((e environment) name value)
  (setf (gethash name (slot-value e 'values)) value))

(defmethod resolve ((e environment) name)
  (with-slots (enclosing) e
    (multiple-value-bind (value present)
        (gethash (lexeme name) (slot-value e 'values))
      (cond (present value)
            (enclosing (resolve enclosing name))
            (t (throw-runtime-error
                name
                (format nil "Undefined variable '~a'." (lexeme name))))))))

(defmethod assign ((e environment) name value)
  (with-slots (enclosing values) e
    (cond ((cadr (multiple-value-list (gethash (lexeme name) values)))
           (setf (gethash (lexeme name) values) value))
          (enclosing (assign enclosing name value))
          (t (throw-runtime-error
              name
              (format nil "Undefined variable '~a'." (lexeme name)))))))
