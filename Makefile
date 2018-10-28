.default: sync

.all: update sync

update:
	$(eval DATE := $(shell date))
	$(MAKE) -C .emacs.d update; git add . && git commit -m "update [$(DATE)]"

sync:
	@git pull --rebase
	@git push origin master
	@cd .emacs.d && cask || exit 1
