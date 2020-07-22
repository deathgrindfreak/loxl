(in-package :interpreter)

(defclass lox-callable () ())

(defgeneric call-fun (lox-callable interpreter arguments))

(defgeneric arity (lox-callable))

(defclass lox-function (lox-callable)
  ((declaration :initarg :declaration
                :reader fn-declaration)
   (closure :initarg :closure
            :reader closure)))

(defmethod arity ((l lox-function))
  (with-slots ((params ast::params)) (fn-declaration l)
    (length params)))

(defmethod call-fun ((l lox-function) (i interpreter) arguments)
  (let ((env (make-instance 'environment :enclosing (closure l))))
    (with-slots ((params ast::params) (body ast::body)) (fn-declaration l)
      (loop for param in params
            for arg in arguments
            do (define env (lexeme param) arg t))
      (handler-bind ((return-condition #'(lambda (c)
                                           (return-from call-fun (return-value c)))))
        (execute-block i body env)))))

;; Native clock function
(defclass clock (lox-callable) ())

(defmethod arity ((l clock)) 0)

(defmethod call-fun ((l clock) (i interpreter) arguments)
  (/ (get-universal-time) 1000.0))
