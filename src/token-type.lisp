(in-package :token-type)

(defparameter *keywords*
  '(:and :class :else :false :fun :for :if :nil :or
    :print :return :super :this :true :var :while
    :break))

(defparameter *token-type*
  (append
   *keywords*
   '(;; Single-character tokens.
     :left-paren :right-paren :left-brace :right-brace
     :comma :dot :minus :plus :semicolon :slash :star
     :question :colon

     ;; One or two character tokens.
     :bang :bang-equal
     :equal :equal-equal
     :greater :greater-equal
     :less :less-equal

     ;; Literals.
     :identifier :string :number

     :eof)))

(defparameter *keyword-map*
  (let ((m (make-hash-table :size (length *keywords*))))
    (loop for keyword in *keywords*
          do (setf (gethash keyword m) keyword)
          finally (return m))))

(defun is-keyword (ident)
  (gethash (as-keyword ident) *keyword-map*))
