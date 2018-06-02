.default: compile

compile:
	find inits -name "*.el" | xargs emacs -batch -f batch-byte-compile

clean:
	find inits -name "*.elc" | xargs rm

.PHONY: update
update:
	$(eval DATE := $(shell date))
	@git add . && git commit -m "update [$(DATE)]"
