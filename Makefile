EMACS ?= emacs
CASK ?= cask

all: test

test: clean
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean

unit:
	${CASK} exec ert-runner

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile *.el core/*.el

clean:
	rm -f *.elc core/*.elc

.PHONY:	all test unit
