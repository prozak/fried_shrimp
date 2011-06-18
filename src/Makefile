
default: fried_shrimp

fried_shrimp : compiler.fasl vm.fasl main.fasl match.fasl
	sbcl --eval '(progn (load "vm.fasl") (load "compiler.fasl") (load "main.fasl") (sb-ext:save-lisp-and-die "fried_shrimp" :executable t :toplevel (quote main)))'

compiler.fasl : compiler.lisp
	sbcl --eval '(progn (compile-file "compiler.lisp") (quit))'

vm.fasl : vm.lisp
	sbcl --eval '(progn (compile-file "vm.lisp") (quit))'

match.fasl : match.lisp compiler.fasl vm.fasl
	sbcl --eval '(progn (load "compiler.fasl") (load "vm.fasl") (compile-file "match.lisp") (quit))'

main.fasl : main.lisp match.fasl vm.fasl compiler.fasl
	sbcl --eval '(progn (load "vm.fasl") (load "compiler.fasl") (compile-file "main.lisp") (quit))'

clean:
	rm -f *.fasl

.PHONY: clean default
