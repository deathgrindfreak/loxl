(in-package :ast)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun slots->defclass-slots (slots)
    (loop for slot in slots
          collect (let ((s (if (listp slot) (cadr slot) slot)))
                    `(,s :initarg ,(as-keyword s)
                         :reader ,s
                         ,@(when (listp slot) (list :type (car slot)))))))

  (defun slot-values (slots)
    (mapcar #'(lambda (x) (if (listp x) (cadr x) x)) slots)))

(defmacro define-ast (base subclasses)
  `(progn
     (defclass ,base () ())

     ,@(apply #'append
              (mapcar
               #'(lambda (subclass)
                   (let ((name (car subclass))
                         (slots (cdr subclass))
                         (inst-name (gensym)))
                     (list
                       `(defclass ,name (,base)
                         ,(slots->defclass-slots slots))

                       ;; Pretty print data object
                       `(defmethod print-object ((,inst-name ,name) out)
                         (with-slots ,(slot-values slots) ,inst-name
                           (print-unreadable-object (,inst-name out :type t)
                             (format out "~{~a~^ ~}" (list ,@(slot-values slots)))))))))
               subclasses))

     ;; Export the base class
     (export ',base)

     ;; Export subclasses and their slots
     ,@(apply #'append
              (mapcar
               #'(lambda (subclass)
                   (cons
                    `(export ',(car subclass))
                    (mapcar #'(lambda (slot) `(export ',slot))
                            (slot-values (cdr subclass)))))
               subclasses))))

(define-ast expr
  ((binary (expr left) (token operator) (expr right))
   (grouping (expr group))
   (literal value)
   (unary (token operator) (expr right))))

(defgeneric print-ast (expr)
  (:documentation "Pretty prints the AST"))

(defun parenthesize (name &rest args)
  (format nil "(~a ~{~a~^ ~})"
          name
          (mapcar #'print-ast args)))

(defmethod print-ast ((b binary))
  (parenthesize (lexeme (operator b)) (left b) (right b)))

(defmethod print-ast ((u unary))
  (parenthesize (lexeme (operator u)) (right u)))

(defmethod print-ast ((g grouping))
  (parenthesize "group" (group g)))

(defmethod print-ast ((l literal))
  (or (value l) "nil"))





;; (macroexpand-1
;;  '(define-ast expr
;;    ((binary (expr left) (token operator) (expr right))
;;     (grouping (expr expression))
;;     (literal value)
;;     (unary (token operator) (expr right)))))

;; (defun make-binary (l op r)
;;   (make-instance 'binary
;;                  :operator op
;;                  :left l
;;                  :right r))

;; (defun make-unary (op r)
;;   (make-instance 'unary
;;                  :operator op
;;                  :right r))

;; (defun make-literal (v)
;;   (make-instance 'literal :value v))

;; (defun make-grouping (e)
;;   (make-instance 'grouping :expression e))

;; (defun make-token (tk type)
;;   (make-instance 'token
;;                  :line 1
;;                  :literal nil
;;                  :type type
;;                  :lexeme tk))

;; (print-ast
;;  (make-binary
;;   (make-unary (make-token "-" :minus)
;;               (make-literal 123))
;;   (make-token "*" :star)
;;   (make-grouping (make-literal 45.67))))
