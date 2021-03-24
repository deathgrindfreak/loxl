(in-package :parser)

(defclass parser ()
  ((tokens :initarg :tokens)
   (current :initform 0)
   (had-parse-error :initform nil)
   (in-restart :initform nil)))

(define-condition parser-error (error)
  ((token :initarg :token :reader error-token)
   (message :initarg :message :reader parser-error-message)
   (parsed-expr :initarg :parsed-expr :reader parsed-expr)
   (had-parse-error :initarg :had-parse-error :reader had-parse-error)
   (in-restart :initarg :in-restart :reader in-restart)
   (open-blocks :initarg :open-blocks :reader open-blocks)))

;; TODO We could discern between a parser and repl parser in order to not count blocks
(defmethod throw-parser-error ((p parser) message &optional parsed-expr)
  (with-slots (tokens had-parse-error in-restart) p
    (error 'parser-error
           :token (peek p)
           :message message
           :parsed-expr parsed-expr
           :had-parse-error had-parse-error
           :in-restart in-restart
           :open-blocks (reduce (lambda (c tk)
                                  (let ((type (token-type tk)))
                                    (+ c
                                       (case type
                                         (:left-brace 1)
                                         (:right-brace -1)
                                         (otherwise 0)))))
                                tokens
                                :initial-value 0))))

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

(defmethod consume ((p parser) type message &optional parsed-expr)
  (if (check p type)
      (advance p)
      (throw-parser-error p message parsed-expr)))

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

(defmethod finish-call ((p parser) callee)
  (let ((arguments (unless (check p :right-paren)
                     (loop for total-arguments = 0 then (1+ total-arguments)
                           collect (assignment p)
                           do (when (>= total-arguments 255)
                                (throw-parser-error p "Cannot have more than 255 arguments."))
                           while (match p :comma)))))
    (make-instance 'call :callee callee
                         :paren (consume p :right-paren "Expect ')' after arguments.")
                         :arguments arguments)))

(defmethod call ((p parser))
  (let ((expr (primary p)))
    (loop do
      (if (match p :left-paren)
          (setf expr (finish-call p expr))
          (return expr)))))

(defmethod unary ((p parser))
  (if (match p :bang :minus)
      (make-instance 'unary
                     :operator (previous p)
                     :right (unary p))
      (call p)))

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
    (consume p :semicolon "Expect ';' after expression." expr)
    (make-instance 'expr-stmt :expression expr)))

(defparameter *in-function-declaration* nil
  "Keeps track of whether we're inside of a function call for return params")

(defmethod fun-declaration ((p parser) kind)
  (let ((name (consume p :identifier
                       (format nil "Expect ~a name." kind))))
    (consume p :left-paren (format nil "Expect '(' after ~a name." kind))
    (let ((parameters (unless (check p :right-paren)
                        (loop for param-count = 0 then (1+ param-count)
                              when (>= param-count 255)
                                do (throw-parser-error p "Cannot have more than 255 parameters.")
                              collect (consume p :identifier "Expect parameter name.")
                              while (match p :comma)))))
      (consume p :right-paren "Expect ')' after parameters.")
      (consume p :left-brace (format nil "Expect '{' before ~a body." kind))
      (push name *in-function-declaration*)
      (let ((body (block-statement p)))
        (pop *in-function-declaration*)
        (make-instance 'fun-stmt
                       :name name
                       :params parameters
                       :body body)))))

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
    (consume p :right-paren "Expect ')' after 'condition'.")
    (let ((then-branch (statement p))
          (else-branch))
      (when (match p :else)
        (setf else-branch (statement p)))
      (make-instance 'if-stmt
                     :condition condition
                     :then-branch then-branch
                     :else-branch else-branch))))

(defmethod while-statement ((p parser))
  (consume p :left-paren "Expect '(' after 'while'.")
  (let ((condition (expression p)))
    (consume p :right-paren "Expect ')' after 'condition'.")
    (make-instance 'while-stmt
                   :condition condition
                   :body (statement p))))

(defmethod for-statement ((p parser))
  (consume p :left-paren "Expect '(' after 'for'.")
  (let ((initializer (cond ((match p :semicolon) nil)
                           ((match p :var) (var-declaration p))
                           (t (expr-statement p))))
        (condition (when (not (check p :semicolon))
                     (expression p))))
    (consume p :semicolon "Expect ';' after loop condition.")
    (let ((increment (when (not (check p :right-paren))
                       (expression p))))
      (consume p :right-paren "Expect ')' after for clauses.")
      (let ((body (statement p)))
        ;; When increment exists, append increment to the end of the body
        (when increment
          (setf body
                (make-instance
                 'block-stmt
                 :statements (list body
                                   (make-instance
                                    'expr-stmt
                                    :expression increment)))))

        ;; Ensure that condition will evaluate to true always if missing
        (when (null condition)
          (setf condition (make-instance 'literal :value t)))

        ;; Wrap body in a while statement
        (setf body (make-instance 'while-stmt
                                  :condition condition
                                  :body body))

        ;; Append initializer before while loop if it exists
        (when initializer
          (setf body
                (make-instance
                 'block-stmt
                 :statements (list initializer body))))
        body))))

(defmethod break-statement ((p parser))
  (let ((keyword (previous p)))
    (consume p :semicolon "Expect ';' after break statement.")
    (make-instance 'loop-keyword-stmt :keyword keyword)))

(defmethod return-statement ((p parser))
  (when (not *in-function-declaration*)
    ;; Show the "return" keyword in the error and not the body
    (decf (slot-value p 'current))
    (throw-parser-error p "Cannot return from top-level code."))
  (let ((keyword (previous p))
        (value))
    (unless (check p :semicolon)
      (setf value (expression p)))
    (consume p :semicolon "Expect ';' after return value.")
    (make-instance 'return-stmt
                   :keyword keyword
                   :value value)))

(defmethod statement ((p parser))
  (cond ((match p :while) (while-statement p))
        ((match p :for) (for-statement p))
        ((match p :break) (break-statement p))
        ((match p :if) (if-statement p))
        ((match p :print) (print-statement p))
        ((match p :return) (return-statement p))
        ((match p :left-brace)
         (make-instance 'block-stmt
                        :statements (block-statement p)))
        (t (expr-statement p))))

(defmethod declaration-stmt ((p parser))
  (cond ((match p :fun) (fun-declaration p "function"))
        ((match p :var) (var-declaration p))
        (t (statement p))))

(defmethod parse-statement ((p parser))
  ;; Save the current position in case we need to revert
  (let ((current-position (slot-value p 'current)))
    (restart-case (let ((stmt (declaration-stmt p)))
                    ;; We need to know if we've parsed a statement
                    ;; when interpreting pure expressions in the repl
                    (setf (slot-value p 'in-restart) t)
                    stmt)
      ;; Can't parse anymore, sync and print out errors
      (sync-after-parse-error ()
        (setf (slot-value p 'had-parse-error) t)
        (synchronize p))

      ;; Error happened at the end, so perhaps user can continue adding input
      (restart-with-new-line (new-tokens)
        (with-slots (tokens current) p
          ;; We need to know if we're in a restart when interpreting pure expressions in the repl
          (setf (slot-value p 'in-restart) t)
          (vector-pop tokens) ; remove the last eof token
          (setf current current-position)
          (loop for token across new-tokens
                do (vector-push-extend token tokens))
          (parse-statement p)))

      ;; When a pure expression is parsed, turn it into a print statement
      (restart-with-implicit-stmt ()
        (with-slots (tokens current) p
          (setf current current-position)
          (vector-pop tokens) ; remove the last eof token
          (setf tokens
               (let ((print-token
                       (make-instance 'token
                                      :line 1
                                      :type :print
                                      :literal :print
                                      :lexeme "print"))
                     (semicolon-token
                       (make-instance 'token
                                      :line 1
                                      :type :semicolon
                                      :literal nil
                                      :lexeme ";"))
                     (eof-token
                       (make-instance 'token
                                      :line 1
                                      :type :eof
                                      :literal nil
                                      :lexeme ""))
                     (new-tokens (make-array (+ 2 (array-dimension tokens 0))
                                             :fill-pointer 0
                                             :adjustable t)))
                 (vector-push-extend print-token new-tokens)
                 (loop for token across tokens
                       do (vector-push-extend token new-tokens))
                 (vector-push-extend semicolon-token new-tokens)
                 (vector-push-extend eof-token new-tokens)
                 new-tokens))
          (parse-statement p))))))

(defmethod parse ((p parser))
  (let ((stmts (loop while (not (is-at-end p))
                     collect (parse-statement p))))
    (unless (had-error p) stmts)))
