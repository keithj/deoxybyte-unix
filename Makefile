
# This Makefile is, for the most part, a wrapper around the Lisp build
# system ASDF. It provides targets for building non-Lisp components,
# such as texinfo documentation.


.PHONY:	all fasl doc clean

default: all

all: fasl doc

fasl:
	sbcl --noinform --noprint \
	--eval "(progn (asdf:oos 'asdf:compile-op :cl-mmap) (quit))"

doc:
	sbcl --noinform --noprint \
	--eval "(progn (asdf:oos 'asdf:cldoc-op :cl-mmap) (quit))"

test:
	sbcl --noinform --noprint \
	--eval "(progn (asdf:oos 'asdf:test-op :cl-mmap) (quit))"

coverage:
	sbcl --dynamic-space-size 512 --load src/test-coverage-report.lisp

clean:
	find . -name \*.fasl -exec rm {} \;
