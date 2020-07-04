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

(defmethod run ((l loxl) (i interpreter) in)
  (let* ((tokens (scan-tokens (make-instance 'scanner :source in)))
         (parse-tree (parse (make-instance 'parser :tokens tokens))))
    (when parse-tree
      (interpret i parse-tree))))

(defmethod run-with-error-handling ((l loxl) (i interpreter) in)
  (handler-case (run l i in)
    (scanner-error (e) (handle-scanner-error l e))
    (runtime-error (e) (handle-runtime-error l e))))

(defun indent-line (current-indent)
  (loop repeat current-indent do (princ "..")
        finally (princ ". ")))

(defmethod restart-with-new-line ((l loxl) e)
  (indent-line (open-blocks e))
  (with-input-from-string (in (read-line))
    (handler-case (scan-tokens (make-instance 'scanner :source in))
      (scanner-error (e) (handle-scanner-error l e)))))

(defmethod parser-restarts ((l loxl) e)
  (if (eq :eof (token-type (error-token e)))
      (invoke-restart 'restart-with-new-line
                      (restart-with-new-line l e))
      (progn
        (handle-parse-error l e)
        (invoke-restart 'sync-after-parse-error))))

(defmethod run-prompt ((l loxl))
  (let ((interpreter (make-instance 'interpreter)))
    (loop do
      (format t "> ")
      (with-input-from-string (in (read-line))
        (handler-bind ((parser-error #'(lambda (e) (parser-restarts l e))))
          (run-with-error-handling l interpreter in))))))

(defmethod run-file ((l loxl) file-name)
  (with-open-file (in file-name)
    (let ((interpreter (make-instance 'interpreter)))
      (handler-bind ((parser-error
                       #'(lambda (e)
                           (handle-parse-error l e)
                           (invoke-restart 'sync-after-parse-error))))
        (run-with-error-handling l interpreter in)))))

(defun main (&optional args)
  (let ((l (make-instance 'loxl)))
    (cond ((> (length args) 1)
           (error "Usage: loxl [script]"))
          ((= 1 (length args))
           (run-file l (car args)))
          (t (run-prompt l)))))
