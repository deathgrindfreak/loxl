(in-package :interpreter)

(defclass environment ()
  ((values :initform (make-hash-table :test 'equal))))

(defmethod define ((e environment) name value)
  (setf (gethash name (slot-value e 'values)) value))

(defmethod resolve ((e environment) name)
  (multiple-value-bind (value present)
      (gethash (lexeme name) (slot-value e 'values))
    (if present
        value
        (throw-runtime-error name
                             (format nil "Undefined variable '~a'." (lexeme name))))))

(defmethod assign ((e environment) name value)
  (with-slots (values) e
    (if (cadr (multiple-value-list (gethash (lexeme name) values)))
        (setf (gethash (lexeme name) values) value)
        (throw-runtime-error name
                             (format nil "Undefined variable '~a'." (lexeme name))))))
