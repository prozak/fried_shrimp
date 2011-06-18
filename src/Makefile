
default: fried_shrimp

fried_shrimp : compiler.fasl vm.fasl main.fasl match.fasl
	sbcl --eval '(progn (load "vm.fasl") (load "compiler.fasl") (load "main.fasl") (sb-ext:save-lisp-and-die "fried_shrimp" :executable t :toplevel (quote main) :purify t))'

compiler.fasl : compiler.lisp
	sbcl --eval '(progn (compile-file "compiler.lisp") (quit))'

vm.fasl : vm.lisp
	sbcl --eval '(progn (compile-file "vm.lisp") (quit))'

match.fasl : match.lisp compiler.fasl vm.fasl
	sbcl --eval '(progn (load "compiler.fasl") (load "vm.fasl") (compile-file "match.lisp") (quit))'

main.fasl : main.lisp match.fasl vm.fasl compiler.fasl
	sbcl --eval '(progn (load "vm.fasl") (load "compiler.fasl") (compile-file "main.lisp") (quit))'

sent: sent/src
	mkdir -p sent/src
	cp *.lisp sent/src
	echo "#!/bin/sh" > sent/install
	echo "cd src" >> sent/install
	echo "make" >> sent/install
	echo "cd .." >> sent/install
	echo "#!/bin/sh" > sent/run
	echo "src/fried_shrimp" >> sent/run
	cd sent; tar czf sent.tar.gz *

clean:
	rm -f *.fasl
	rm -rf sent

.PHONY: clean default
