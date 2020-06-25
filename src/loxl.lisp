(in-package :loxl)

(defclass loxl () ())

(defmethod report-msg ((l loxl) line where message)
  (format t "[line ~a] Error~a: ~a~%" line where message))

(defmethod handle-scanner-error ((l loxl) (e scanner-error))
  (report-msg l (error-line e) "" (scanner-error-message e)))

(defmethod handle-parse-error ((l loxl) (e parser-error))
  (let ((tk (error-token e))
        (msg (parser-error-message e)))
    (if (eq :eof (token-type tk))
        (report-msg l (token-line tk) " at end" msg)
        (report-msg l (token-line tk) (format nil " at '~a'" (lexeme tk)) msg))))

(defmethod handle-runtime-error ((l loxl) (e runtime-error))
  (let ((tk (runtime-error-token e))
        (msg (runtime-error-message e)))
    (report-msg l (token-line tk) "" msg)))

(defmethod run ((l loxl) in)
  (let* ((tokens (scan-tokens (make-instance 'scanner :source in)))
         (parse-tree (parse (make-instance 'parser :tokens tokens))))
    (when parse-tree
      (interpret parse-tree))))

(defmethod run-prompt ((l loxl))
  (format t "> ")
  (loop for line = (read-line)
        do (with-input-from-string (in line)
             (handler-bind ((parser-error
                              #'(lambda (e)
                                  (handle-parse-error l e)
                                  (invoke-restart 'sync-after-parse-error)))
                            (scanner-error #'(lambda (e) (handle-scanner-error l e)))
                            (runtime-error #'(lambda (e) (handle-runtime-error l e))))
               (run l in))
             (format t "> "))))

(defmethod run-file ((l loxl) file-name)
  (with-open-file (in file-name)
    ;; TODO Need error handling here
    (run l in)))

(defun main (&optional args)
  (let ((l (make-instance 'loxl)))
    (cond ((> (length args) 1)
           (error "Usage: loxl [script]"))
          ((= 1 (length args))
           (run-file l (car args)))
          (t (run-prompt l)))))
