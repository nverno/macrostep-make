curl  ?= curl
emacs ?= emacs
wget  ?= wget
RM    = rm -rf

all:
test: test/macrostep-make-tests.el
	$(emacs) -Q -batch --eval '(progn (push "." load-path))' \
	-L . -l ert -l test/macrostep-make-tests.el              \
	-f ert-run-tests-batch-and-exit

README.md : el2markdown.el macrostep-make.el
	$(emacs) -batch -l $< macrostep-make.el -f el2markdown-write-readme

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~
