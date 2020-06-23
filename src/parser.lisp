(in-package :parser)

(defclass parser ()
  ((tokens :initarg :tokens)
   (current :initform 0)))

(define-condition parser-error (error)
  ((token :initarg :token :reader error-token)
   (message :initarg :message :reader parser-error-message)))

(defmethod throw-parser-error ((p parser) message)
  (error 'parser-error :token (peek p) :message message))

(defmethod peek ((p parser))
  (with-slots (tokens current) p
    (aref tokens current)))

(defmethod previous ((p parser))
  (with-slots (tokens current) p
    (aref tokens (1- current))))

(defmethod is-at-end ((p parser))
  (eq (token-type (peek p)) :eof))

(defmethod advance ((p parser))
  (when (not (is-at-end p))
    (incf (slot-value p 'current)))
  (previous p))

(defmethod check ((p parser) type)
  (and (not (is-at-end p))
       (eq (token-type (peek p)) type)))

(defmethod match ((p parser) &rest types)
  (and (member-if #'(lambda (tp) (check p tp))
                  types)
       (advance p)))

(defmethod consume ((p parser) type message)
  (if (check p type)
      (advance p)
      (throw-parser-error p message)))

(defmethod synchronize ((p parser))
  (advance p)
  (loop while (or (not (is-at-end p))
                  (eq (token-type (previous p)) :semicolon)
                  (member (token-type (peek p))
                          '(:class :fun :var :for
                            :if :while :print :return)))
        do (advance p)))

(defmethod primary ((p parser))
  (cond
    ((match p :false) (make-instance 'literal :value nil))
    ((match p :true) (make-instance 'literal :value t))
    ((match p :nil) (make-instance 'literal :value nil))
    ((match p :number :string)
     (make-instance 'literal :value (literal (previous p))))
    ((match p :left-paren)
     (let ((expr (expression p)))
       (consume p :right-paren "Expect ')' after expression.")
       (make-instance 'grouping :group expr)))
    (t (throw-parser-error p "Expect expression."))))

(defmethod unary ((p parser))
  (if (match p :bang :minus)
      (make-instance 'unary
                     :operator (previous p)
                     :right (unary p))
      (primary p)))

(defmacro define-binary-parser (name subexpr tokens)
  (let ((p (gensym))
        (expr (gensym))
        (operator (gensym))
        (right (gensym)))
    `(defmethod ,name ((,p parser))
       (let ((,expr (,subexpr ,p)))
         (loop while (match ,p ,@tokens)
               do (let ((,operator (previous ,p))
                        (,right (,subexpr ,p)))
                    (setf ,expr (make-instance 'binary
                                               :left ,expr
                                               :operator ,operator
                                               :right ,right)))
               finally (return ,expr))))))

(define-binary-parser multiplication unary (:slash :star))

(define-binary-parser addition multiplication (:minus :plus))

(define-binary-parser comparison addition
  (:greater :greater-equal :less :less-equal))

(define-binary-parser equality comparison (:bang-equal :equal-equal))

(defmethod expression ((p parser))
  (equality p))

;; (defmethod parse ((p parser))
;;   (handler-case (expression p)
;;     (parser-error () nil)))

(defmethod parse ((p parser))
  (expression p))
