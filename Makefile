.default: update

.all: update sync

update:
	$(eval DATE := $(shell date))
	@git add . && git commit -m "update [$(DATE)]"

sync: update
	@git pull --rebase
	@git push origin master
