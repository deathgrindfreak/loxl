(in-package :interpreter)

(define-condition runtime-error (error)
  ((token :initarg :token :reader runtime-error-token)
   (message :initarg :message :reader runtime-error-message)))

(defun throw-runtime-error (token message)
  (error 'runtime-error
         :token token
         :message message))

(defun check-number-operand (operator operand)
  (unless (numberp operand)
    (throw-runtime-error operator "Operand must be a number.")))

(defun check-number-operands (operator left right)
  (unless (and (numberp left) (numberp right))
    (throw-runtime-error operator "Operands must be a numbers.")))

(defgeneric evaluate (expr)
  (:documentation "Evaluates an expression"))

(defmethod evaluate ((tn ternary))
  (with-slots (ast::predicate (te ast::true-expr) (fe ast::false-expr)) tn
    (let ((p (evaluate ast::predicate)))
      (if p (evaluate te) (evaluate fe)))))

(defmethod evaluate ((b binary))
  (with-slots ((left ast::left) (operator ast::operator) (right ast::right)) b
    (let ((l (evaluate left))
          (r (evaluate right)))
      (flet ((op (o)
               (check-number-operands operator l r)
               (funcall o l r)))
        (case (token-type operator)
          ;; Right now just return the rightmost operand
          (:comma r)
          (:minus (op #'-))
          (:plus (cond
                   ((and (numberp l) (numberp r))
                    (+ l r))
                   ((or (stringp l) (stringp r))
                    (format nil "~a~a" l r))
                   (t (throw-runtime-error
                       operator
                       "Operands must be two numbers or two strings."))))
          (:slash (if (zerop r)
                      (throw-runtime-error
                       operator
                       "Division by zero.")
                      (op #'/)))
          (:star (op #'*))
          (:greater (op #'>))
          (:greater-equal (op #'>=))
          (:less (op #'<))
          (:less-equal (op #'<=))
          (:equal-equal (equal l r))
          (:bang-equal (not (equal l r)))
          (otherwise nil))))))

(defmethod evaluate ((u unary))
  (with-slots (ast::right ast::operator) u
    (let ((expr (evaluate ast::right)))
      (case (token-type ast::operator)
        (:minus
         (check-number-operand ast::operator ast::right)
         (- expr))
        (:bang (not expr))
        (otherwise nil)))))

(defmethod evaluate ((g grouping))
  (evaluate (slot-value g 'ast::group)))

(defmethod evaluate ((l literal))
  (slot-value l 'ast::value))

(defun stringify (object)
  (cond ((null object) "nil")
        ((numberp object) (format nil "~d"
                                  (if (and (rationalp object)
                                           (not (integerp object)))
                                      (float object)
                                      object)))
        (t (format nil "~a" object))))

(defmethod interpret ((e expr))
  (let ((value (evaluate e)))
    (stringify value)))
