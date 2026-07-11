EMACS ?= emacs
EL_FILES ?= $(wildcard *.el)

.PHONY: lint lint-compile test clean

lint:
	@$(EMACS) --batch -L . -l lint.el -- $(EL_FILES)

lint-compile:
	@$(EMACS) --batch -L . -l lint.el -- --compile $(EL_FILES)

test:
	@$(EMACS) --batch -Q -L . -L test \
		--eval "(setq load-prefer-newer t)" \
		-l test/magnus-provider-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
