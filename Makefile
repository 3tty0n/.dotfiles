.default: sync

.all: update sync

update:
	$(eval DATE := $(shell date))
	@cd .emacs.d; $(MAKE) update; cd ..; git add . && git commit -m "update [$(DATE)]"

sync:
	@git pull --rebase
	@git push origin master
	@cd .emacs.d && cask || exit 1
