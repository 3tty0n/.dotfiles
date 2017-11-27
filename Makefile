update:
	@git add . && git commit -m "update [$(date)]"
	@git push -u origin master


sync:
	@git pull --rebase
