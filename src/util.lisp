(in-package :util)

(defun as-keyword (sym) (intern (string-upcase sym) :keyword))

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
     ,@body))
