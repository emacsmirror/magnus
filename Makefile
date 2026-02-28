EMACS ?= emacs
EL_FILES ?= $(wildcard *.el)

.PHONY: lint lint-compile clean

lint:
	@$(EMACS) --batch -L . -l lint.el -- $(EL_FILES)

lint-compile:
	@$(EMACS) --batch -L . -l lint.el -- --compile $(EL_FILES)

clean:
	rm -f *.elc
