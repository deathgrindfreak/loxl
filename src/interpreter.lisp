(in-package :interpreter)

(defclass interpreter ()
  ((globals :initform (make-instance 'environment)
                :accessor globals)
   (environment :accessor environment)
   (loop-count :initform 0 :accessor loop-count)
   (is-breaking :initform nil :accessor is-breaking)))

(defmethod initialize-instance :after ((i interpreter) &rest args)
  (declare (ignore args))
  (setf (environment i) (globals i))
  (define (globals i) "clock" (make-instance 'clock) t))

(defun check-number-operand (operator operand)
  (unless (numberp operand)
    (throw-runtime-error operator "Operands must be numbers.")))

(defun check-number-operands (operator left right)
  (unless (and (numberp left) (numberp right))
    (throw-runtime-error operator "Operands must be numbers.")))

(defmethod perform-break ((i interpreter))
  (when (is-breaking i)
      (progn
        (setf (is-breaking i) nil)
        t)))

(defgeneric evaluate (interpreter expr)
  (:documentation "Evaluates an expression"))

(define-condition return-condition (condition)
  ((value :initarg :value :reader return-value)))

(defmethod evaluate ((i interpreter) (stmt return-stmt))
  (with-slots ((stmt-value ast::value)) stmt
    (signal 'return-condition
            :value (when stmt-value
                     (evaluate i stmt-value)))))

(defmethod evaluate ((i interpreter) (stmt fun-stmt))
  (with-slots ((name ast::name)) stmt
    (let ((function (make-instance 'lox-function
                                   :declaration stmt
                                   :closure (environment i))))
      (define (environment i) (lexeme name) function t)
      nil)))

(defmethod evaluate ((i interpreter) (stmt anon-fun-stmt))
  (with-slots ((name ast::name)) stmt
    (let ((function (make-instance 'lox-function
                                   :declaration stmt
                                   :closure (environment i))))
      (define (environment i) (lexeme name) function t)
      function)))

(defmethod evaluate ((i interpreter) (expr call))
  (with-slots ((callee ast::callee)
               (paren ast::paren)
               (args ast::arguments))
      expr
    (let ((f (evaluate i callee)))
      (unless (subtypep (type-of f) 'lox-callable)
        (throw-runtime-error
         paren
         "Can only call functions and classes."))
      (unless (= (arity f) (length args))
        (throw-runtime-error
         paren
         (format nil "Expected ~a arguments but got ~a." (arity f) (length args))))
      (call-fun f i (loop for arg in args
                          collect (evaluate i arg))))))

(defmethod evaluate ((i interpreter) (stmt if-stmt))
  (with-slots ((condition ast::condition)
               (thenbr ast::then-branch)
               (elsebr ast::else-branch))
      stmt
    (if (evaluate i condition)
        (evaluate i thenbr)
        (when elsebr
          (evaluate i elsebr))))
  nil)

(defmethod evaluate ((i interpreter) (stmt loop-keyword-stmt))
  (with-slots ((keyword ast::keyword)) stmt
    (when (zerop (loop-count i))
      (throw-runtime-error keyword "Break statement found outside of loop."))
    keyword))

(defmethod evaluate ((i interpreter) (s while-stmt))
  (incf (loop-count i))
  (unwind-protect
       (with-slots ((condition ast::condition) (body ast::body)) s
         (loop while (evaluate i condition)
               do (if (perform-break i)
                      (return)
                      (evaluate i body))))
    (decf (loop-count i)))
  nil)

(defmethod execute-block ((i interpreter) statements env)
  (let ((previous (environment i)))
    (unwind-protect
         (progn
           (setf (environment i) env)
           (loop for statement in statements
                 do (let ((stmt (evaluate i statement)))
                      (when (and stmt (eq :break (token-type stmt)))
                        (setf (is-breaking i) t)
                        (return)))))
      (setf (environment i) previous))))

(defmethod evaluate ((i interpreter) (e block-stmt))
  (with-slots ((statements ast::statements)) e
    (execute-block i statements (make-instance 'environment
                                               :enclosing (environment i)))
    nil))

(defmethod evaluate ((i interpreter) (e assign))
  (with-slots ((value ast::value) (name ast::name)) e
    (let ((v (evaluate i value)))
      (assign (environment i) name v)
      v)))

(defmethod evaluate ((i interpreter) (e var-expr))
  (resolve (environment i) (slot-value e 'ast::name)))

(defmethod evaluate ((i interpreter) (stmt var-stmt))
  (with-slots ((name ast::name)
               (init ast::initializer)
               (was-initialized ast::was-initialized))
      stmt
    (let ((value))
      (when init
        (setf value (evaluate i init)))
      (define (environment i) (lexeme name) value was-initialized)))
  nil)

(defmethod evaluate ((i interpreter) (e expr-stmt))
  (evaluate i (slot-value e 'ast::expression))
  nil)

(defmethod evaluate ((i interpreter) (e print-stmt))
  (format t "~a~%"
          (stringify
           (evaluate i (slot-value e 'ast::expression))))
  nil)

(defmethod evaluate ((i interpreter) (expr logical))
  (with-slots ((left ast::left) (op ast::operator) (right ast::right)) expr
    (let ((left (evaluate i left)))
      (cond ((and (eq :or (token-type op)) left) left)
            ((and (eq :and (token-type op)) (not left)) left)
            (t (evaluate i right))))))

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
                   ((and (or (numberp l) (stringp l))
                         (or (numberp r) (stringp r)))
                    (format nil "~a~a" l r))
                   (t (throw-runtime-error
                       operator
                       "Operands must be two numbers or two strings."))))
          (:slash (if (and (numberp r) (zerop r))
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
  (with-slots ((right ast::right) (op ast::operator)) u
    (let ((expr (evaluate i right)))
      (case (token-type op)
        (:minus
         (check-number-operand op expr)
         (- expr))
        (:bang (not expr))
        (otherwise nil)))))

(defmethod evaluate ((i interpreter) (g grouping))
  (evaluate i (slot-value g 'ast::group)))

(defmethod evaluate ((i interpreter) (l literal))
  (slot-value l 'ast::value))

(defun stringify (object)
  (cond ((null object) "nil")
        ((eq t object) "true")
        ((numberp object) (format nil "~d"
                                  (if (and (rationalp object)
                                           (not (integerp object)))
                                      (float object)
                                      object)))
        (t (format nil "~a" object))))

(defmethod interpret ((i interpreter) statements)
  (dolist (statement statements)
    (evaluate i statement)))
