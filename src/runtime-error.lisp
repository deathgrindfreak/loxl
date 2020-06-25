(in-package :interpreter)

(define-condition runtime-error (error)
  ((token :initarg :token :reader runtime-error-token)
   (message :initarg :message :reader runtime-error-message)))

(defun throw-runtime-error (token message)
  (error 'runtime-error
         :token token
         :message message))
