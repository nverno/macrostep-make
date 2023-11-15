
.PHONY: all test
all:
	@

# test: test/macrostep-make-tests.el
# 	$(emacs) -Q -batch --eval '(progn (push "." load-path))' \
# 	-L . -l ert -l test/macrostep-make-tests.el              \
# 	-f ert-run-tests-batch-and-exit

.PHONY: clean distclean
clean:
	$(RM) *~ *.core *.o *.out *.exe 

distclean: clean
	$(RM) -r $$(git ls-files --others --ignored --exclude-standard)
