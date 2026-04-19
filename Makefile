EMACS ?= emacs

EL_FILES = emcp-prompts.el emcp-resources.el emcp-tools.el emcp.el

LOAD_PATH="(progn \
  (require 'package) \
  (package-initialize) \
  (add-to-list 'load-path default-directory))"

.PHONY: test lint byte-compile checkdoc docs clean

lint: byte-compile checkdoc

byte-compile:
	$(EMACS) --batch \
		--eval $(LOAD_PATH) \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(EL_FILES)

checkdoc:
	$(EMACS) --batch \
		--eval "(require 'checkdoc)" \
		--eval "(let ((n 0)) \
		           (advice-add (quote display-warning) :before \
		                       (lambda (type &rest _) (when (eq type (quote emacs)) (setq n (1+ n)))) \
		                       (quote ((name . count-checkdoc-warnings)))) \
		           (mapc (function checkdoc-file) (list $(patsubst %,\"%\",$(EL_FILES)))) \
		           (when (> n 0) (message \"checkdoc: found docstring issues (see warnings above)\") (kill-emacs 1)))"

TEST_SELECTOR ?= t
test: clean lint
	$(EMACS) --batch \
		--eval $(LOAD_PATH) \
		-l emcp-tests.el \
		--eval "(ert-run-tests-batch-and-exit '$(TEST_SELECTOR))"

# Clean before to ensure that the docs do not use a stale compiled version of component
# docstrings.
docs: clean
	$(EMACS) --batch \
		--eval $(LOAD_PATH) \
		--eval "(require 'emcp)" \
		--eval "(require 'org)" \
		--eval "(setq org-confirm-babel-evaluate nil)" \
		--eval "(with-current-buffer (find-file-noselect \"README.org\") \
		           (org-babel-map-src-blocks nil \
		             (when (equal (cdr (assq :exports (nth 2 (org-babel-get-src-block-info)))) \"none\") \
		               (org-babel-execute-src-block))) \
		           (save-buffer))"

clean:
	rm -f *.elc
