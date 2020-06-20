(in-package :loxl)

(defclass loxl ()
  ((had-error :initform nil)))

(defmethod report-msg ((l loxl) line where message)
  (with-slots (had-error) l
    (format t "[line ~a] Error~a: ~a~%" line where message)
    (setf had-error t)))

(defmethod report-error ((l loxl) line message)
  (report-msg l line "" message))

(defmethod run ((l loxl) in))

(defmethod run-prompt ((l loxl))
  (format t "> ")
  (loop for line = (read-line)
        do (with-slots (had-error) l
             (with-input-from-string (in line)
               (run l in)

               ;; Don't kill the session
               (setf had-error nil)
               (format t "> ")))))

(defmethod run-file ((l loxl) file-name)
  (with-open-file (in file-name :element-type '(unsigned-byte 8))
    (run l in)
    (when had-error
      (error "TODO"))))

(defmethod main ((l loxl) args)
  (cond ((> (length args) 1)
         (error "Usage: loxl [script]"))
        ((= 1 (length args))
         (run-file l (car args)))
        (t (run-prompt l))))
