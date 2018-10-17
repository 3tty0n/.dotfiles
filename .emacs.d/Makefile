EMACS := emacs
BATCHFLAG := -batch -f batch-byte-compile

compile:
	find ./site-lisp -name '*.el' | xargs $(EMACS) $(BATCHFLAG)

clean:
	find . -name '*.elc' -delete

update:
	git add . && git commit -m "[emacs] update"

