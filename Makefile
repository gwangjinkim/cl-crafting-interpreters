LISP ?= sbcl

test:
	$(LISP) --non-interactive \
		--eval '(ql:quickload :fiveam)' \
		--eval '(push #P"$(PWD)/" asdf:*central-registry*)' \
		--eval '(asdf:test-system :cl-lox-treewalk)'

treewalk:
	$(LISP) --non-interactive \
		--eval '(push #P"$(PWD)/" asdf:*central-registry*)' \
		--eval '(asdf:load-system :cl-lox-treewalk)' \
		--eval '(sb-ext:save-lisp-and-die "lox-tw" :toplevel (function cl-lox-treewalk:main) :executable t)'

.PHONY: test treewalk
