EMACS = emacs

check: compile
	$(EMACS) -q -batch -eval "(check-declare-file \"guid.el\")" 2>&1 | grep -e "Checking"
	$(EMACS) -q -batch -l guid.el -l guid-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -l guid.elc -l guid-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -q -batch -f batch-byte-compile guid.el

clean:
	rm -f *.elc
