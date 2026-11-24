EMACS ?= emacs

EL_FILES = emcp-uri.el emcp-core.el emcp-confirm.el emcp-http.el \
  emcp-resources.el \
  emcp-screenshot.el \
  emcp-server-manager.el \
  emcp-tools.el emcp-tools-eval.el emcp-tools-send-keys.el \
  emcp.el

# Dependencies are installed into .deps, independent of the user's packages.  Run
# `make update-deps` to reinstall their newest versions.
LOAD_PATH="(progn \
  (require 'package) \
  (setq package-user-dir (expand-file-name \".deps\" default-directory)) \
  (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\")) \
  (package-initialize) \
  (unless (package-installed-p 'http-server) \
    (package-vc-install '(http-server :url \"https://codeberg.org/martenlienen/http-server.el.git\"))) \
  (unless (package-installed-p 'elisp-refs) \
    (unless package-archive-contents (package-refresh-contents)) \
    (package-install 'elisp-refs)) \
  (unless (package-installed-p 'magit-section) \
    (unless package-archive-contents (package-refresh-contents)) \
    (package-install 'magit-section)) \
  (add-to-list 'load-path default-directory))"

.PHONY: test lint byte-compile checkdoc format docs clean update-deps

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

# Load all code first, so that the indentation declarations of EMCP's and the
# tests' macros apply.
format:
	$(EMACS) --batch \
		--eval $(LOAD_PATH) \
		--eval "(add-to-list 'load-path (expand-file-name \"test\"))" \
		$(patsubst %,-l %,$(EL_FILES) $(wildcard test/*.el)) \
		--eval "(dolist (file (list $(patsubst %,\"%\",$(EL_FILES) $(wildcard test/*.el)))) \
		           (with-current-buffer (find-file-noselect file) \
		             (setq-local indent-tabs-mode nil) \
		             (indent-region (point-min) (point-max)) \
		             (delete-trailing-whitespace) \
		             (when (buffer-modified-p) \
		               (message \"Formatted %s\" file) \
		               (save-buffer))))"

TEST_FILES ?= $(wildcard test/*-tests.el)
TEST_SELECTOR ?= t
test: clean lint
	$(EMACS) --batch \
		--eval $(LOAD_PATH) \
		--eval "(add-to-list 'load-path (expand-file-name \"test\"))" \
		$(patsubst %,-l %,$(TEST_FILES)) \
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

update-deps:
	rm -rf .deps
	$(EMACS) --batch --eval $(LOAD_PATH)
