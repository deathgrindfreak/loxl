(in-package :interpreter)

(defclass environment ()
  ((enclosing :initform nil :initarg :enclosing)
   (uninitialized-vars :initform nil)
   (values :initform (make-hash-table :test 'equal))))

(defmethod define ((e environment) name value was-initialized)
  (unless was-initialized
    (push name (slot-value e 'uninitialized-vars)))
  (setf (gethash name (slot-value e 'values)) value))

(defmethod resolve ((e environment) name)
  (with-slots (enclosing uninitialized-vars) e

    ;; Disallow Uninitialized variables
    (when (member (lexeme name) uninitialized-vars :test #'equal)
      (throw-runtime-error
       name
       (format nil "Uninitialized variable '~a'." (lexeme name))))

    (multiple-value-bind (value present)
        (gethash (lexeme name) (slot-value e 'values))
      (cond (present value)
            (enclosing (resolve enclosing name))
            (t (throw-runtime-error
                name
                (format nil "Undefined variable '~a'." (lexeme name))))))))

(defmethod assign ((e environment) name value)
  (with-slots (enclosing values uninitialized-vars) e
    (cond ((cadr (multiple-value-list (gethash (lexeme name) values)))
           (setf uninitialized-vars
                 (delete (lexeme name) uninitialized-vars :test #'equal))
           (setf (gethash (lexeme name) values) value))
          (enclosing (assign enclosing name value))
          (t (throw-runtime-error
              name
              (format nil "Undefined variable '~a'." (lexeme name)))))))
