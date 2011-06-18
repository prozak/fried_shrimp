
default: fried_shrimp

fried_shrimp : compiler.fasl vm.fasl main.fasl
	sbcl --eval '(progn (load "vm.fasl") (load "compiler.fasl") (load "main.fasl") (sb-ext:save-lisp-and-die "fried_shrimp" :executable t :toplevel (quote main)))'

compiler.fasl : compiler.lisp
	sbcl --eval '(progn (compile-file "compiler.lisp") (quit))'

vm.fasl : vm.lisp
	sbcl --eval '(progn (compile-file "vm.lisp") (quit))'

main.fasl : main.lisp
	sbcl --eval '(progn (compile-file "main.lisp") (quit))'