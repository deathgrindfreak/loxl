(in-package :token-type)

(defconstant +token-type+
  '(;; Single-character tokens.
    :left-paren :right-paren :left-brace :right-brace
    :comma :dot :minus :plus :semicolon :slash :star

    ;; One or two character tokens.
    :bang :bang-equal
    :equal :equal-equal
    :greater :greater-equal
    :less :less-equal

    ;; Literals.
    :identifier :string :number

    ;; Keywords.
    :and :class :else :false :fun :for :if :nil :or
    :print :return :super :this :true :var :while

    :eof))
