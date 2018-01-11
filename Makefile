LISP?=sbcl

build:
	$(LISP) --load readline-example.asd \
		--eval '(ql:quickload :readline-example)' \
		--eval '(asdf:make :readline-example)' \
		--eval '(quit)'
