(in-package :scanner)

(defclass scanner ()
  ((source :initarg :source)
   (tokens :initform nil
           :accessor tokens)
   (eof-from-stream :initform nil)
   (char-buffer :initform nil)
   (line :initform 1)))

(define-condition scanner-error (error)
  ((line :initarg :line :reader line)
   (message :initarg :message :reader message)))

(defmethod throw-scanner-error ((s scanner) message)
  (error 'scanner-error
         :line (slot-value s 'line)
         :message message))

(defgeneric advance (scanner &key collect-char &allow-other-keys))

(defmethod advance ((s scanner) &key (collect-char t))
  (with-slots (source eof-from-stream char-buffer) s
    (let ((c (read-char source nil nil)))
      (unless c (setf eof-from-stream t))
      (when collect-char (push c char-buffer))
      (or c #\null))))

(defmethod peek ((s scanner))
  (with-slots (source) s
    (if (is-at-end s)
        #\null
        (or (peek-char nil source nil nil)
            #\null))))

(defmethod pop-buffer ((s scanner))
  (pop (slot-value s 'char-buffer)))

(defmethod is-at-end ((s scanner))
  (slot-value s 'eof-from-stream))

(defmethod match ((s scanner) expected)
  (with-slots (char-buffer) s
    (and (not (is-at-end s))
         (char= (peek s) expected)
         (advance s))))

(defmethod add-token ((s scanner) type &optional literal)
  (with-slots (char-buffer line) s
    (push (make-instance 'token
                         :type type
                         :lexeme (coerce (reverse char-buffer) 'string)
                         :literal literal
                         :line line)
          (tokens s))
    (setf char-buffer nil)))

(defmethod str ((s scanner))
  (loop while (and (char/= (peek s) #\")
                   (not (is-at-end s)))
        do (progn
             (when (char= (peek s) #\newline)
               (incf (slot-value s 'line)))
             (advance s)))

  (when (is-at-end s)
    (throw-scanner-error s "Unterminated string."))

  (advance s)
  (add-token s :string
             (let ((full-string (coerce (reverse (slot-value s 'char-buffer))
                                        'string)))
               (subseq full-string 1 (1- (length full-string))))))

(defmethod scan-token ((s scanner))
  (let ((c (advance s)))
    ;; Since we stream instead of consume the entire file, duck out earlier if EOF appears
    (when (not (is-at-end s))
      (case c
        (#\( (add-token s :left-paren))
        (#\) (add-token s :right-paren))
        (#\{ (add-token s :left-brace))
        (#\} (add-token s :right-brace))
        (#\, (add-token s :comma))
        (#\. (add-token s :dot))
        (#\- (add-token s :minus))
        (#\+ (add-token s :plus))
        (#\; (add-token s :semicolon))
        (#\* (add-token s :star))
        (#\! (add-token s (if (match s #\=) :bang-equal :bang)))
        (#\= (add-token s (if (match s #\=) :equal-equal :equal)))
        (#\< (add-token s (if (match s #\=) :less-equal :less)))
        (#\> (add-token s (if (match s #\=) :greater-equal :greater)))
        (#\/ (if (match s #\/)
                 (progn
                   ;; Remove double slash
                   (pop-buffer s)
                   (pop-buffer s)
                   ;; Skip comments
                   (loop while (and (char/= #\newline (peek s))
                                    (not (is-at-end s)))
                         do (advance s :collect-char nil)))
                 (add-token s :slash)))
        (#\newline (with-slots (line) s
                     (pop-buffer s)
                     (incf line)))
        (#\space (pop-buffer s))
        (#\return (pop-buffer s))
        (#\tab (pop-buffer s))
        (#\" (str s))
        (otherwise (throw-scanner-error s "Unexpected char."))))))

(defmethod scan-tokens ((s scanner))
  (loop while (not (is-at-end s)) do (scan-token s))
  (push (make-instance 'token
                       :type :eof
                       :lexeme ""
                       :literal nil
                       :line (slot-value s 'line))
        (tokens s)))

(with-open-file (in "../t/data/simple-scan.lox")
  (let ((s (make-instance 'scanner :source in)))
    (scan-tokens s)
    (tokens s)))
