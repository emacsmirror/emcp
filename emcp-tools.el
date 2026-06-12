;;; emcp-tools.el --- Tool definitions for EMCP -*- lexical-binding: t -*-

;; Author: Marten Lienen <ml@martenlienen.com>

;;; License

;; This file is part of EMCP.

;; EMCP is free software: you can redistribute it and/or modify it under the terms of the
;; GNU General Public License as published by the Free Software Foundation, either version
;; 3 of the License, or (at your option) any later version.

;; EMCP is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with EMCP.  If
;; not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; MCP tools as defined by the spec [1].
;;
;; [1] https://modelcontextprotocol.io/specification/2025-11-25/server/tools

;;; Code:

(require 'cl-lib)
(require 'elisp-refs)
(require 'find-func)
(require 'info)
(require 'lisp-mnt)
(require 'url-util)

(require 'emcp-core)

(emcp-deftool emcp-tools-apropos
    ((pattern "Regular expression to search for")
     (kind "Restrict results to a kind of symbol (\"any\", \"function\", \"command\", \"macro\", \"variable\", \"custom\", \"face\", \"feature\" or \"widget\")"
           :default "any"))
  "Search for Emacs symbols matching a regular expression."
  :name "apropos"
  :description "Search for symbols matching a regular expression, optionally restricted to a specific kind."
  (cl-labels ((symbol-kinds (sym)
                (let (kinds)
                  (when (fboundp sym)
                    (push (cond ((commandp sym) "command")
                                ((macrop sym) "macro")
                                (t "function"))
                          kinds))
                  (when (boundp sym)
                    (push (if (custom-variable-p sym) "custom" "variable")
                          kinds))
                  (when (facep sym) (push "face" kinds))
                  (when (featurep sym) (push "feature" kinds))
                  (when (widgetp sym) (push "widget" kinds))
                  (nreverse kinds)))
              (format-symbol (sym)
                (if (equal kind "any")
                    (format "%s (%s)"
                            (symbol-name sym)
                            (string-join (symbol-kinds sym) ", "))
                  (symbol-name sym))))
    (if-let* ((predicate (pcase kind
                           ("any" (lambda (sym) (not (keywordp sym))))
                           ("function" #'fboundp)
                           ("command" #'commandp)
                           ("macro" #'macrop)
                           ("variable" (lambda (sym)
                                         (and (boundp sym) (not (keywordp sym)))))
                           ("custom" #'custom-variable-p)
                           ("face" #'facep)
                           ("feature" #'featurep)
                           ("widget" #'widgetp))))
        (if-let* ((symbols (apropos-internal pattern predicate)))
            `((content . [((type . "text")
                           (text . ,(mapconcat #'format-symbol symbols "\n")))]))
          `((content . [((type . "text")
                         (text . "No matching symbols found."))])))
      `((content . [((type . "text")
                     (text . ,(format "Unknown kind %s" kind)))])
        (isError . t)))))

(defun emcp-tools--find-definition (symbol type)
  "Find the definition of SYMBOL of TYPE.

TYPE is one of `defun', `defvar', `defface', `feature'.

Return a plist (:file FILE :line LINE :source SOURCE) or nil if not
found."
  ;; Read the definitions without applying file- or dir-local
  ;; variables, so that we do not prompt the user or modify Emacs
  ;; state.
  (let ((enable-local-variables nil)
        (enable-dir-local-variables nil))
    (if (and (eq type 'feature) (featurep symbol))
        (let ((file (find-library-name (symbol-name symbol))))
          `( :file ,file
             :source ,(with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string))))
      (if-let* ((file (symbol-file symbol type)))
          (pcase-let ((`(,buf . ,pos)
                       (find-function-search-for-symbol symbol
                                                        (if (eq type 'defun) nil type)
                                                        file)))
            (when (and buf pos)
              (with-current-buffer buf
                (goto-char pos)
                (let* ((file (or (buffer-file-name buf) (buffer-name buf)))
                       (line (line-number-at-pos pos))
                       (end (save-excursion
                              (forward-sexp 1)
                              (point)))
                       (source (buffer-substring-no-properties
                                pos (min end (+ pos 2000)))))
                  (list :file file :line line :source source)))))
        ;; symbol-file returned nil. Is it a C function or variable?
        (when-let* ((c-file (cond
                             ((eq type 'defun)
                              (help-C-file-name symbol 'subr))
                             ((eq type 'defvar)
                              (help-C-file-name symbol 'var)))))
          (if find-function-C-source-directory
              (let* ((emacs-dir (file-name-directory find-function-C-source-directory))
                     (file (expand-file-name c-file emacs-dir))
                     (type (if (eq type 'defun) nil type)))
                (pcase-let ((`(,buf . ,pos) (find-function-C-source symbol file type)))
                  (if (and buf pos)
                      (with-current-buffer buf
                        (goto-char pos)
                        (let* ((file (or (buffer-file-name buf) (buffer-name buf)))
                               (line (line-number-at-pos pos))
                               (end (save-excursion
                                      (if (eq type 'defvar)
                                          (progn
                                            (forward-sexp)
                                            (point))
                                        (save-restriction
                                          ;; Heuristic to narrow to the definition of the
                                          ;; DEFUN macro
                                          (forward-sexp)
                                          (forward-line)
                                          (narrow-to-defun)
                                          (point-max)))))
                               (source (buffer-substring-no-properties pos end)))
                          (list :file file :line line :source source)))
                    (list :file (format "C source (%s)" c-file)
                          :source "Definition not found in C source. The source version may not match the running Emacs."))))
            (list :file (format "C source (%s)" c-file)
                  :source (format "C source is not available. Set `find-function-C-source-directory' to the Emacs source directory to enable navigating to C definitions."))))))))

(emcp-deftool emcp-tools-find-definition
    ((symbol "Name of the symbol to look up.")
     (kind "Restrict to a kind of definition (\"any\", \"function\", \"command\", \"macro\", \"variable\", \"custom\", \"face\", \"feature\" or \"widget\")."
           :default "any"))
  "Find the source definition of an Emacs symbol."
  :name "find-definition"
  :description "Find the definition of a symbol, returning its location and source code."
  (let ((types (pcase kind
                 ("any" '(defun defvar defface feature))
                 ((or "function" "command" "macro") '(defun))
                 ((or "variable" "custom") '(defvar))
                 ("face" '(defface))
                 ("feature" '(feature))
                 ("widget" (list 'unsupported kind))
                 (_ (list 'unknown kind)))))
    (pcase types
      (`(unknown ,k)
       `((content . [((type . "text")
                      (text . ,(format "Unknown kind %s" k)))])
         (isError . t)))
      (`(unsupported ,k)
       `((content . [((type . "text")
                      (text . ,(format "Cannot find definitions for kind %s." k)))])
         (isError . t)))
      (_
       (cl-flet ((format-def (def)
                   `((type . "text")
                     (text . ,(if (plist-get def :line)
                                  (format "%s:%d\n%s"
                                          (plist-get def :file)
                                          (plist-get def :line)
                                          (plist-get def :source))
                                (format "%s\n%s"
                                        (plist-get def :file)
                                        (plist-get def :source)))))))
         (if-let* ((sym (intern-soft symbol))
                   (results (delq nil (mapcar (lambda (type)
                                                (emcp-tools--find-definition sym type))
                                              types))))
             `((content . ,(vconcat (mapcar #'format-def results))))
           `((content . [((type . "text")
                          (text . ,(format "No definition found for %s." symbol)))]))))))))

(defun emcp-tools--find-references-match-fn (kind sym)
  "Return a function matching references to SYM in a buffer for KIND.

The returned function takes a buffer and returns a list of \(_ START END)
matches as in `elisp-refs--read-and-find'."
  (pcase kind
    ((or "function" "command")
     (lambda (buf) (elisp-refs--read-and-find buf sym #'elisp-refs--function-p)))
    ("macro"
     (lambda (buf) (elisp-refs--read-and-find buf sym #'elisp-refs--macro-p)))
    ((or "variable" "custom")
     (lambda (buf) (elisp-refs--read-and-find buf sym #'elisp-refs--variable-p)))
    (_
     (lambda (buf) (elisp-refs--read-and-find-symbol buf sym)))))

(defun emcp-tools--ref-snippet (buf start-pos end-pos)
  "Return (LINE . SOURCE) for the reference at START-POS to END-POS in BUF.

LINE is the 1-based line number of START-POS.  SOURCE is the buffer
content from the start of the line at START-POS to the end of the line
at END-POS."
  (with-current-buffer buf
    (save-excursion
      (goto-char start-pos)
      (let* ((line (line-number-at-pos))
             (line-start (line-beginning-position))
             (line-end (progn (goto-char end-pos) (line-end-position))))
        (cons line (buffer-substring-no-properties line-start line-end))))))

(defun emcp-tools--unindent (source)
  "Rigidly unindent SOURCE so its first line begins at column 0.

The leading whitespace of the first line is stripped from every line
that begins with the same prefix, preserving relative indentation."
  (if (string-match "\\`\\([ \t]+\\)" source)
      (replace-regexp-in-string
       (concat "^" (regexp-quote (match-string 1 source)))
       "" source)
    source))

(defun emcp-tools--cap-lines (source max-lines)
  "Truncate SOURCE to at most MAX-LINES lines.

If MAX-LINES is nil, return SOURCE unchanged.  Otherwise, if SOURCE has
more than MAX-LINES lines, return the first MAX-LINES lines followed by
a `...' continuation marker on its own line."
  (if (not max-lines)
      source
    (let ((lines (split-string source "\n")))
      (if (<= (length lines) max-lines)
          source
        (concat (mapconcat #'identity (seq-take lines max-lines) "\n")
                "\n...")))))

(defun emcp-tools--format-ref (line source max-lines)
  "Format a single reference at LINE with SOURCE snippet.

When MAX-LINES is non-nil, the snippet is truncated to at most that
many lines."
  (format "%d: %s" line
          (emcp-tools--cap-lines (emcp-tools--unindent source) max-lines)))

(defcustom emcp-tools-find-references-max-lines 3
  "Maximum number of lines per snippet in `emcp-tools-find-references' output.

If nil, snippets are not truncated.  If an integer, snippets that span
more lines are cut off after that many lines and a `...' continuation
marker is appended."
  :group 'emcp
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Maximum lines")))

(emcp-deftool emcp-tools-find-references
    ((symbol "Name of the symbol to look up.")
     (kind "Restrict to a kind of reference (\"any\", \"function\", \"command\", \"macro\", \"variable\", \"custom\", \"face\", \"feature\" or \"widget\")."
           :default "any"))
  "Find references to an Emacs symbol in all loaded elisp files."
  :name "find-references"
  (if-let* ((sym (intern-soft symbol)))
      (let* ((match-fn (emcp-tools--find-references-match-fn kind sym))
             (paths (elisp-refs--loaded-paths))
             (bufs (mapcar #'elisp-refs--contents-buffer paths))
             (elisp-refs-verbose nil))
        (unwind-protect
            (let* ((forms-and-bufs (elisp-refs--search-1 bufs match-fn))
                   (per-file
                    (mapcar
                     (lambda (entry)
                       (pcase-let* ((`(,matches . ,buf) entry)
                                    (path (with-current-buffer buf
                                            elisp-refs--path)))
                         (cons (abbreviate-file-name path)
                               (mapcar (lambda (m)
                                         (pcase-let ((`(,_ ,start ,end) m))
                                           (emcp-tools--ref-snippet buf start end)))
                                       matches))))
                     forms-and-bufs)))
              (if (null per-file)
                  `((content . [((type . "text")
                                 (text . ,(format "No references to %s." symbol)))]))
                (let ((text (mapconcat
                             (lambda (entry)
                               (let ((path (car entry))
                                     (refs (cdr entry)))
                                 (concat path "\n"
                                         (mapconcat
                                          (lambda (ref)
                                            (emcp-tools--format-ref
                                             (car ref) (cdr ref)
                                             emcp-tools-find-references-max-lines))
                                          refs "\n"))))
                             per-file "\n\n")))
                  `((content . [((type . "text") (text . ,text))])))))
          (dolist (buf bufs) (kill-buffer buf))))
    `((content . [((type . "text")
                   (text . ,(format "No symbol named %s." symbol)))])
      (isError . t))))

(emcp-deftool emcp-tools-describe
    ((symbol "Name of the symbol to describe.")
     (kind "Restrict to a kind of definition (\"any\", \"function\", \"command\", \"macro\", \"variable\", \"custom\", \"face\", \"feature\" or \"widget\")."
           :default "any"))
  "Describe an Emacs symbol."
  :name "describe"
  :description "Get the documentation for a symbol."
  (let ((types (pcase kind
                 ("any" '(function variable face feature))
                 ((or "function" "command" "macro") '(function))
                 ((or "variable" "custom") '(variable))
                 ("face" '(face))
                 ("feature" '(feature)))))
    (cl-labels ((get-doc (sym type)
                  (pcase type
                    ('function (and (fboundp sym) (documentation sym)))
                    ('variable (documentation-property sym 'variable-documentation))
                    ('face (documentation-property sym 'face-documentation))
                    ('feature (and (featurep sym)
                                   (lm-commentary
                                    (find-library-name (symbol-name sym)))))))
                (format-doc (sym type)
                  (when-let* ((doc (get-doc sym type)))
                    (format "[%s]\n%s" (if (equal kind "any") (symbol-name type) kind) doc))))
      (if (not types)
          `((content . [((type . "text")
                         (text . ,(format "Unknown kind %s" kind)))])
            (isError . t))
        (if-let* ((sym (intern-soft symbol))
                  (results (delq nil (mapcar (lambda (type) (format-doc sym type)) types))))
            `((content . [((type . "text")
                           (text . ,(mapconcat #'identity results "\n\n")))]))
          `((content . [((type . "text")
                         (text . ,(format "No documentation found for %s."
                                          symbol)))])))))))

(defun emcp-tools--info-search-manual (pattern manual)
  "Search indices of MANUAL for PATTERN.

Return a list of (MANUAL ENTRY NODE) triples."
  ;; Index entry format: "* ENTRY:  NODE." - same pattern as Info-apropos-matches
  (let ((re (format "\n\\* +\\([^\n]*%s[^\n]*\\):[ \t]+\\([^\n]+\\)\\." pattern))
        matches)
    (with-temp-buffer
      (Info-mode)
      (condition-case nil
          (progn
            (Info-find-node manual "Top")
            (dolist (index-node (Info-index-nodes))
              (Info-find-node manual index-node)
              (goto-char (point-min))
              (while (re-search-forward re nil t)
                (push (list manual (match-string 1) (match-string 2))
                      matches))))
        (error nil)))
    (nreverse matches)))

(emcp-deftool emcp-tools-info-search
    ((pattern "Regular expression to search for in Info indices")
     (manual "Restrict search to this manual name, e.g. \"elisp\" or \"emacs\""
             :default nil))
  "Search Info manual indices for a regular expression."
  :name "info-search"
  (let ((matches (if manual
                     ;; Restrict to single manual file for faster search
                     (emcp-tools--info-search-manual pattern manual)
                   ;; Info-apropos-matches returns t instead of nil
                   ;; when there are no matches
                   (let ((result (Info-apropos-matches pattern t)))
                     (and (listp result) result)))))
    (if matches
        `((content . ,(vconcat
                       (mapcar
                        (lambda (m)
                          (pcase-let ((`(,man ,entry ,node) m))
                            `((type . "resource_link")
                              (uri . ,(format "info://%s/%s"
                                              (url-hexify-string man)
                                              (url-hexify-string node)))
                              (name . ,(substring-no-properties entry))
                              (description . ,(format "(%s)%s" man node))
                              (mimeType . "text/plain"))))
                        matches))))
      `((content . [((type . "text")
                     (text . "No matching entries found."))])))))

;;; Variables

(emcp-deftool emcp-tools-get-variable
    ((name "Symbol"))
  "Read the global default value of an Emacs variable.

Returns the value's printed representation via `prin1-to-string'.
Always returns the global default value, even for buffer-local
variables."
  :name "get-variable"
  (if-let* ((sym (intern-soft name))
            ((default-boundp sym)))
      `((content . [((type . "text")
                     (text . ,(prin1-to-string (default-value sym))))]))
    `((content . [((type . "text")
                   (text . ,(format "Variable %s is not bound." name)))])
      (isError . t))))

(emcp-deftool emcp-tools-set-variable
    ((name "Symbol")
     (value "New value as a Lisp literal, e.g. =42=, =\"hi\"=, =(1 2 3)=, =t=, =nil=."))
  "Set the global default value of an Emacs variable."
  :name "set-variable"
  (cl-flet ((err-result (msg)
              `((content . [((type . "text") (text . ,msg))])
                (isError . t))))
    (if-let* ((sym (intern-soft name))
              ((default-boundp sym)))
        (pcase (condition-case err
                   (read-from-string value)
                 (error (error-message-string err)))
          ((and (pred stringp) msg)
           (err-result (format "Failed to parse value: %s" msg)))
          ((and `(,_ . ,pos) (guard (/= pos (length value))))
           (err-result (format "Trailing characters after value at position %d." pos)))
          (`(,form . ,_)
           (condition-case err
               (progn
                 (set-default sym form)
                 `((content . [((type . "text")
                                (text . ,(format "Set %s to %s"
                                                 name (prin1-to-string form))))])))
             (error
              (err-result (format "Failed to set: %s"
                                  (error-message-string err)))))))
      (err-result (format "Variable %s is not bound." name)))))

(provide 'emcp-tools)
;;; emcp-tools.el ends here
