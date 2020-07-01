(in-package :parser)

(defclass parser ()
  ((tokens :initarg :tokens)
   (current :initform 0)
   (had-parse-error :initform nil)))

(define-condition parser-error (error)
  ((token :initarg :token :reader error-token)
   (message :initarg :message :reader parser-error-message)))

(defmethod throw-parser-error ((p parser) message)
  (error 'parser-error :token (peek p) :message message))

(defmethod had-error ((p parser))
  (slot-value p 'had-parse-error))

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
  (loop while (not (or (is-at-end p)
                       (eq (token-type (previous p)) :semicolon)
                       (member (token-type (peek p))
                               '(:class :fun :var :for
                                 :if :while :print :return))))
        do (advance p)))

(defmethod primary ((p parser))
  (cond
    ;; TODO We might need some other mechanism here for false
    ((match p :false) (make-instance 'literal :value nil))
    ((match p :true) (make-instance 'literal :value t))
    ((match p :nil) (make-instance 'literal :value nil))
    ((match p :number :string)
     (make-instance 'literal :value (literal (previous p))))
    ((match p :left-paren)
     (let ((expr (expression p)))
       (consume p :right-paren "Expect ')' after expression.")
       (make-instance 'grouping :group expr)))
    ((match p :identifier) (make-instance 'var-expr :name (previous p)))
    (t (throw-parser-error p "Expect expression."))))

(defmethod unary ((p parser))
  (if (match p :bang :minus)
      (make-instance 'unary
                     :operator (previous p)
                     :right (unary p))
      (primary p)))

(defmacro define-binary-parser (name &body body)
  (with-gensyms (p expr operator right)
    (let ((class-name (getf body :class))
          (subexpr (getf body :operand))
          (tokens (getf body :operators)))
      `(defmethod ,name ((,p parser))
         (let ((,expr (,subexpr ,p)))
           (loop while (match ,p ,@tokens)
                 do (let ((,operator (previous ,p))
                          (,right (,subexpr ,p)))
                      (setf ,expr (make-instance ',class-name
                                                 :left ,expr
                                                 :operator ,operator
                                                 :right ,right)))
                 finally (return ,expr)))))))

(define-binary-parser multiplication
  :class binary
  :operand unary
  :operators (:slash :star))

(define-binary-parser addition
  :class binary
  :operand multiplication
  :operators (:minus :plus))

(define-binary-parser comparison
  :class binary
  :operand addition
  :operators (:greater :greater-equal :less :less-equal))

(define-binary-parser equality
  :class binary
  :operand comparison
  :operators (:bang-equal :equal-equal))

(define-binary-parser and-expr
  :class logical
  :operand equality
  :operators (:and))

(define-binary-parser or-expr
  :class logical
  :operand and-expr
  :operators (:or))

(defmethod ternary ((p parser))
  (let ((expr (or-expr p)))
    (when (match p :question)
      (let ((true-expr (ternary p)))
        (unless (match p :colon)
          (throw-parser-error p "Expected ':' in ternary."))
        (setf expr (make-instance 'ternary
                                  :predicate expr
                                  :true-expr true-expr
                                  :false-expr (ternary p)))))
    expr))

(defmethod assignment ((p parser))
  (let ((expr (ternary p)))
    (if (match p :equal)
        (let ((equals (previous p))
              (value (assignment p)))
          (if (eq 'var-expr (type-of expr))
              (make-instance 'assign
                             :name (slot-value expr 'ast::name)
                             :value value)
              (error 'parser-error
                     :token equals
                     :message "Invalid assignment target.")))
        expr)))

(define-binary-parser comma
  :class binary
  :operand assignment
  :operators (:comma))

(defmethod expression ((p parser))
  (comma p))

(defmethod print-statement ((p parser))
  (let ((value (expression p)))
    (consume p :semicolon "Expect ';' after value.")
    (make-instance 'print-stmt :expression value)))

(defmethod expr-statement ((p parser))
  (let ((expr (expression p)))
    (consume p :semicolon "Expect ';' after expression.")
    (make-instance 'expr-stmt :expression expr)))

(defmethod var-declaration ((p parser))
  (let ((name (consume p :identifier "Expect variable name."))
        (was-initialized)
        (initializer))
    (when (match p :equal)
      (setf initializer (expression p)
            was-initialized t))
    (consume p :semicolon "Expect ';' after variable declaration.")
    (make-instance 'var-stmt
                   :name name
                   :was-initialized was-initialized
                   :initializer initializer)))

(defmethod block-statement ((p parser))
  (let ((statements
          (loop while (not (or (check p :right-brace)
                               (is-at-end p)))
                collect (declaration-stmt p))))
    (consume p :right-brace "Expect '}' after block.")
    statements))

(defmethod if-statement ((p parser))
  (consume p :left-paren "Expect '(' after 'if'.")
  (let ((condition (expression p)))
    (consume p :right-paren "Expect ')' after 'if'.")
    (let ((then-branch (statement p))
          (else-branch))
      (when (match p :else)
        (setf else-branch (statement p)))
      (make-instance 'if-stmt
                     :condition condition
                     :then-branch then-branch
                     :else-branch else-branch))))

(defmethod statement ((p parser))
  (cond ((match p :if) (if-statement p))
        ((match p :print) (print-statement p))
        ((match p :left-brace)
         (make-instance 'block-stmt
                        :statements (block-statement p)))
        (t (expr-statement p))))

(defmethod declaration-stmt ((p parser))
  (if (match p :var)
      (var-declaration p)
      (statement p)))

(defmethod parse-statement ((p parser))
  ;; Save the current position in case we need to revert
  (let ((current-position (slot-value p 'current)))
    (restart-case (declaration-stmt p)
      ;; Can't parse anymore, sync and print out errors
      (sync-after-parse-error ()
        (setf (slot-value p 'had-parse-error) t)
        (synchronize p))

      ;; Error happened at the end, so perhaps user can continue adding input
      (restart-with-new-line (new-tokens)
        (with-slots (tokens current) p
          (vector-pop tokens) ; remove the last eof token
          (setf current current-position)
          (loop for token across new-tokens
                do (vector-push-extend token tokens))
          (parse-statement p))))))

(defmethod parse ((p parser))
  (let ((stmts (loop while (not (is-at-end p))
                     collect (parse-statement p))))
    (unless (had-error p) stmts)))
