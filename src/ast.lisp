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
  (ternary (expr predicate) (expr true-expr) (expr false-expr))
  (binary (expr left) (token operator) (expr right))
  (grouping (expr group))
  (literal value)
  (unary (token operator) (expr right))
  (var-expr (token name)))

(define-ast stmt
  (block-stmt statements)
  (var-stmt (token name) (expr initializer) (boolean was-initialized))
  (expr-stmt (expr expression))
  (print-stmt (expr expression)))

(defgeneric print-ast (expr)
  (:documentation "Pretty prints the AST"))

(defun parenthesize (name &rest args)
  (format nil "(~a ~{~a~^ ~})"
          name
          (mapcar #'print-ast args)))

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
