build:
	sbcl --eval "(asdf:load-system 'loxl)" \
		 --eval "(sb-ext:save-lisp-and-die \"loxl\" :compression t :toplevel #'loxl:main :executable t)"
