(in-package :ast)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun slots->defclass-slots (slots)
    (loop for slot in slots
          collect (let ((s (if (listp slot) (cadr slot) slot)))
                    `(,s :initarg ,(as-keyword s)
                         ,@(when (listp slot) (list :type (car slot)))))))

  (defun slot-values (slots)
    (mapcar #'(lambda (x) (if (listp x) (cadr x) x)) slots)))

(defmacro define-ast (base &body subclasses)
  `(progn
     (defclass ,base () ())

     ,@(apply #'append
              (mapcar
               #'(lambda (subclass)
                   (destructuring-bind (name &rest slots) subclass
                     (with-gensyms (inst-name)
                       (list
                        `(defclass ,name (,base)
                           ,(slots->defclass-slots slots))

                        ;; Pretty print data object
                        `(defmethod print-object ((,inst-name ,name) out)
                           (with-slots ,(slot-values slots) ,inst-name
                             (print-unreadable-object (,inst-name out :type t)
                               (format out "~{~a~^ ~}" (list ,@(slot-values slots))))))))))
               subclasses))))

(define-ast expr
  (assign (token name) (expr value))
  (logical (expr left) (token operator) (expr right))
  (ternary (expr predicate) (expr true-expr) (expr false-expr))
  (binary (expr left) (token operator) (expr right))
  (call (expr callee) (token paren) (cons arguments))
  (grouping (expr group))
  (literal value)
  (unary (token operator) (expr right))
  (var-expr (token name)))

(define-ast stmt
  (while-stmt (expr condition) (stmt body))
  (loop-keyword-stmt (token keyword))
  (fun-stmt (token name) (cons params) (cons body))
  (return-stmt (token keyword) (expr value))
  (if-stmt (expr condition) (stmt then-branch) (stmt else-branch))
  (block-stmt (cons statements))
  (var-stmt (token name) (expr initializer) (boolean was-initialized))
  (expr-stmt (expr expression))
  (print-stmt (expr expression)))

(defgeneric print-ast (expr)
  (:documentation "Pretty prints the AST"))

(defun parenthesize (name &rest args)
  (if args
      (format nil "(~a ~{~a~^ ~})"
              name
              (mapcar #'print-ast args))
      name))

(defmethod print-ast ((e call))
  (with-slots (callee arguments) e
    (apply #'parenthesize (print-ast callee) arguments)))

(defmethod print-ast ((s loop-keyword-stmt))
  (with-slots (keyword) s
    (parenthesize (lexeme keyword))))

(defmethod print-ast ((s if-stmt))
  (with-slots (condition then-branch else-branch) s
    (if else-branch
        (parenthesize "if" condition then-branch else-branch)
        (parenthesize "if" condition then-branch))))

(defmethod print-ast ((s assign))
  (with-slots (name value) s
    (parenthesize (lexeme name) value)))

(defmethod print-ast ((s expr-stmt))
  (with-slots (expression) s
    (print-ast expression)))

(defmethod print-ast ((s var-expr))
  (with-slots (name) s
    (parenthesize (lexeme name))))

(defmethod print-ast ((s var-stmt))
  (with-slots (name initializer) s
    (parenthesize (lexeme name) initializer)))

(defmethod print-ast ((s block-stmt))
  (with-slots (statements) s
    (apply #'parenthesize "block" statements)))

(defmethod print-ast ((s while-stmt))
  (with-slots (condition body) s
    (parenthesize "while" condition body)))

(defmethod print-ast ((s print-stmt))
  (with-slots (expression) s
    (parenthesize "print" expression)))

(defmethod print-ast ((b ternary))
  (with-slots (predicate true-expr false-expr) b
    (parenthesize "ternary" predicate true-expr false-expr)))

(defmethod print-ast ((b binary))
  (with-slots (left operator right) b
    (parenthesize (lexeme operator) left right)))

(defmethod print-ast ((u unary))
  (with-slots (operator right) u
    (parenthesize (lexeme operator) right)))

(defmethod print-ast ((g grouping))
  (parenthesize "group" (slot-value g 'group)))

(defmethod print-ast ((l literal))
  (or (slot-value l 'value) "nil"))
