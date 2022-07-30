EMACS ?= emacs

all: test

test:
	${MAKE} unit

unit:
	${EMACS} -batch -l ert -L test/ -l test/test-helper.el -l test/all-tests.el -f ert-run-tests-batch-and-exit

clean:
	${RM} -f *.elc core/*.elc

.PHONY:	all test unit
