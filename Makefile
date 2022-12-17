LISP ?= sbcl

demo:
	${LISP} --eval '(ql:quickload :jingle.demo)' \
		--eval '(asdf:make :jingle.demo)' \
                --eval '(quit)'

demo-doc: demo
	./bin/jingle-demo print-doc > docs/jingle-demo.md

demo-docker:
	docker build -t cl-jingle:latest -f Dockerfile.demo .

demo-test:
	./scripts/run-demo-tests.sh

.PHONY: demo demo-doc demo-docker demo-test
