update:
	$(eval DATE := $(shell date))
	@git add . && git commit -m "update [$(DATE)]"
	@git push -u origin master


sync:
	@git pull --rebase
