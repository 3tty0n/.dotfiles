update:
	$(eval DATE := $(shell date))
	@git add . && git commit -m "update [$(DATE)]"

sync:
	@git pull --rebase
