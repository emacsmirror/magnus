EMACS ?= emacs
EL_FILES ?= $(wildcard *.el)
TEST_FILES ?= $(wildcard test/*-tests.el)

.PHONY: lint lint-compile test clean

lint:
	@$(EMACS) --batch -L . -l lint.el -- $(EL_FILES)

lint-compile:
	@$(EMACS) --batch -L . -l lint.el -- --compile $(EL_FILES)

test:
	@$(EMACS) --batch -Q -L . -L test \
		--eval "(setq load-prefer-newer t)" \
		$(foreach file,$(TEST_FILES),-l $(file)) \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
