EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L .

.PHONY: all compile test lint live clean

all: compile test

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile syscall.el

test:
	$(BATCH) -L test -l test/syscall-test.el -f ert-run-tests-batch-and-exit

# checkdoc, and package-lint fetched from MELPA into a throwaway directory.
lint:
	$(BATCH) --eval '(checkdoc-file "syscall.el")'
	$(BATCH) --eval "(progn (setq package-user-dir (make-temp-file \"elpa\" t)) \
	  (require 'package) (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) \
	  (package-initialize) (package-refresh-contents) (package-install 'package-lint))" \
	  -f package-lint-batch-and-exit syscall.el

# Real update against the API into a throwaway cache, then a second run that
# should get 304 for every file.
live:
	$(BATCH) -l syscall \
	  --eval '(setq syscall-cache-directory (expand-file-name "live-cache/" (or (getenv "TMPDIR") "/tmp")))' \
	  --eval '(progn (or (syscall--update-sync) (kill-emacs 1)) (or (syscall--update-sync) (kill-emacs 1)) (syscall-cache-info))'

clean:
	rm -f *.elc test/*.elc
