EMACS ?= emacs
EL_FILES ?= $(wildcard *.el)
TEST_FILES ?= $(wildcard test/*-tests.el)
LINT_FILES ?= $(EL_FILES) $(wildcard test/*.el)

.PHONY: lint lint-compile package-lint test clean

lint:
	@$(EMACS) --batch -L . -l lint.el -- $(LINT_FILES)

lint-compile:
	@$(EMACS) --batch -L . -l lint.el -- --compile $(EL_FILES)

package-lint:
	@$(EMACS) --batch -Q -L . -L test \
		-l test/test-helper.el \
		-l test/package-lint-check.el

test: clean
	@$(EMACS) --batch -Q -L . -L test \
		-l test/test-helper.el \
		$(foreach file,$(TEST_FILES),-l $(file)) \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
