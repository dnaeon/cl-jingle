LISP ?= sbcl

demo:
	${LISP} --eval '(ql:quickload :jingle.demo)' \
		--eval '(asdf:make :jingle.demo)' \
                --eval '(quit)'

demo-doc: demo
	./bin/jingle-demo print-doc > docs/jingle-demo.md

docker-demo:
	docker build -t cl-jingle:latest -f Dockerfile.demo .

.PHONY: demo demo-doc docker-demo
