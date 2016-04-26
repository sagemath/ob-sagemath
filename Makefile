EMACS ?= emacs
CASK ?= $(HOME)/.cask/bin/cask

compile:
	$(CASK) exec $(EMACS) -Q -eval "(setq byte-compile-error-on-warn t)" \
	-batch -f batch-byte-compile ob-sagemath.el

test: clean compile
	$(CASK) exec $(EMACS) -Q -batch -L . -l tests/tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f ob-sagemath.elc

.PHONY: compile test clean
