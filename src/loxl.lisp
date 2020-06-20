(in-package :loxl)

(defclass loxl () ())

(defmethod report-msg ((l loxl) line where message)
  (format nil "[line ~a] Error~a: ~a" line where message))

(defmethod report-error ((l loxl) (e scanner-error))
  (report-msg l (line e) "" (message e)))

(defmethod run ((l loxl) in)
  (let ((s (make-instance 'scanner :source in)))
    (scan-tokens s)))

(defmethod run-prompt ((l loxl))
  (format t "> ")
  (loop for line = (read-line)
        do (with-input-from-string (in line)
             (format t "~a~%"
              (handler-case (run l in)
                (scanner-error (e) (report-error l e))))
             (format t "> "))))

(defmethod run-file ((l loxl) file-name)
  (with-open-file (in file-name :element-type '(unsigned-byte 8))
    (run l in)))

(defmethod main ((l loxl) args)
  (cond ((> (length args) 1)
         (error "Usage: loxl [script]"))
        ((= 1 (length args))
         (run-file l (car args)))
        (t (run-prompt l))))
