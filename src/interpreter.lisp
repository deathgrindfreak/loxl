(in-package :interpreter)

(defclass interpreter ()
  ((environment :initform (make-instance 'environment)
                :reader environment)))

(defun check-number-operand (operator operand)
  (unless (numberp operand)
    (throw-runtime-error operator "Operand must be a number.")))

(defun check-number-operands (operator left right)
  (unless (and (numberp left) (numberp right))
    (throw-runtime-error operator "Operands must be a numbers.")))

(defgeneric evaluate (interpreter expr)
  (:documentation "Evaluates an expression"))

(defmethod evaluate ((i interpreter) (e var-expr))
  (resolve (environment i) (slot-value e 'ast::name)))

(defmethod evaluate ((i interpreter) (e var-stmt))
  (with-slots ((name ast::name) (init ast::initializer)) e
    (let ((value))
      (when init
        (setf value (evaluate i init)))
      (define (environment i) (lexeme name) value)))
  nil)

(defmethod evaluate ((i interpreter) (e expr-stmt))
  (evaluate i (slot-value e 'ast::expression))
  nil)

(defmethod evaluate ((i interpreter) (e print-stmt))
  (format t "~a~%"
          (stringify
           (evaluate i (slot-value e 'ast::expression))))
  nil)

(defmethod evaluate ((i interpreter) (tn ternary))
  (with-slots (ast::predicate (te ast::true-expr) (fe ast::false-expr)) tn
    (let ((p (evaluate i ast::predicate)))
      (if p (evaluate i te) (evaluate i fe)))))

(defmethod evaluate ((i interpreter) (b binary))
  (with-slots ((left ast::left) (operator ast::operator) (right ast::right)) b
    (let ((l (evaluate i left))
          (r (evaluate i right)))
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

(defmethod evaluate ((i interpreter) (u unary))
  (with-slots (ast::right ast::operator) u
    (let ((expr (evaluate i ast::right)))
      (case (token-type ast::operator)
        (:minus
         (check-number-operand ast::operator ast::right)
         (- expr))
        (:bang (not expr))
        (otherwise nil)))))

(defmethod evaluate ((i interpreter) (g grouping))
  (evaluate i (slot-value g 'ast::group)))

(defmethod evaluate ((i interpreter) (l literal))
  (slot-value l 'ast::value))

(defun stringify (object)
  (cond ((null object) "nil")
        ((numberp object) (format nil "~d"
                                  (if (and (rationalp object)
                                           (not (integerp object)))
                                      (float object)
                                      object)))
        (t (format nil "~a" object))))

(defmethod interpret ((i interpreter) statements)
  (loop for statement in statements
        do (evaluate i statement)))
