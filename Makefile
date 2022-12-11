LISP ?= sbcl

demo:
	${LISP} --eval '(ql:quickload :jingle.demo)' \
		--eval '(asdf:make :jingle.demo)' \
                --eval '(quit)'

demo-doc: demo
	./bin/jingle-demo print-doc > docs/jingle-demo.md

.PHONY: demo demo-doc
