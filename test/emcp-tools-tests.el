;;; emcp-tools-tests.el --- Tests for the built-in tools -*- lexical-binding: t -*-

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

;; Tests for the inspection tools in emcp-tools.el.

;;; Code:

(require 'cl-lib)
(require 'emcp-test-helpers)
(require 'emcp-tools)
(require 'ert)

(ert-deftest emcp-tests-apropos-any ()
  (emcp-test-with-tool-response response 'emcp-tools-apropos
                                '((pattern . "^emcp-tools-apropos$"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (string-match-p "emcp-tools-apropos (function)" text)))))

(ert-deftest emcp-tests-apropos-command ()
  ;; emcp-tools-apropos is a function but not a command
  (emcp-test-with-tool-response response 'emcp-tools-apropos
                                '((pattern . "^emcp-tools-apropos$") (kind . "command"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (equal text "No matching symbols found."))))
  ;; emcp-start is a command
  (emcp-test-with-tool-response response 'emcp-tools-apropos
                                '((pattern . "^emcp-start$") (kind . "command"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (equal text "emcp-start")))))

(ert-deftest emcp-tests-apropos-unknown-kind ()
  (emcp-test-with-tool-response response 'emcp-tools-apropos
                                '((pattern . "whatever") (kind . "bogus"))
    (should (eq (alist-get 'isError (alist-get 'result response)) t))))

(ert-deftest emcp-tests-find-definition-function ()
  (emcp-test-with-tool-response response 'emcp-tools-find-definition
                                '((symbol . "emcp--server-destroy"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (string-match-p "emcp-core\\.el" text))
      (should (string-match-p "defun emcp--server-destroy" text)))))

(ert-deftest emcp-tests-find-definition-not-found ()
  (emcp-test-with-tool-response response 'emcp-tools-find-definition
                                '((symbol . "emcp--this-does-not-exist-at-all"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (string-match-p "No definition found" text)))))

(defun emcp-tests--emcp-source-files ()
  "Return the paths of some EMCP source files."
  (mapcar #'find-library-name
          '("emcp" "emcp-core" "emcp-confirm" "emcp-http" "emcp-screenshot")))

(defmacro emcp-tests-with-emcp-refs-scope (&rest body)
  "Run BODY with `elisp-refs--loaded-paths' restricted to a subset of EMCP.

Scanning every file in `load-history' makes `find-references' tests
slow.  Restricting the search to emcp's files brings runtime down from
seconds to milliseconds while still exercising the same code paths."
  (declare (indent 0) (debug (body)))
  `(cl-letf (((symbol-function 'elisp-refs--loaded-paths)
              #'emcp-tests--emcp-source-files))
     ,@body))

(ert-deftest emcp-tests-find-references-function ()
  ;; "function" kind matches call sites and skips the symbol's own `defun'
  ;; definition.
  (emcp-tests-with-emcp-refs-scope
    (emcp-test-with-tool-response response 'emcp-tools-find-references
                                  '((symbol . "emcp--server-destroy") (kind . "function"))
      (let* ((result (alist-get 'result response))
             (text (alist-get 'text (aref (alist-get 'content result) 0))))
        (should-not (alist-get 'isError result))
        (should (string-match-p "(emcp--server-destroy " text))
        (should-not (string-match-p "(defun emcp--server-destroy" text))))))

(ert-deftest emcp-tests-find-references-any ()
  ;; "any" kind matches every occurrence regardless of syntactic position, so
  ;; it includes the `defun' definition line that "function" skips.
  (emcp-tests-with-emcp-refs-scope
    (emcp-test-with-tool-response response 'emcp-tools-find-references
                                  '((symbol . "emcp--server-destroy"))
      (let* ((result (alist-get 'result response))
             (text (alist-get 'text (aref (alist-get 'content result) 0))))
        (should-not (alist-get 'isError result))
        (should (string-match-p "(defun emcp--server-destroy" text))))))

(ert-deftest emcp-tests-find-references-max-lines ()
  ;; emcp--server-build is called with a multi-line argument list in emcp.el,
  ;; so the un-capped snippet spans several lines.  With the limit set to 1,
  ;; only the first line is kept and a continuation marker is appended after
  ;; the first line of the matched form.
  (let ((emcp-tools-find-references-max-lines 1))
    (emcp-tests-with-emcp-refs-scope
      (emcp-test-with-tool-response response 'emcp-tools-find-references
                                    '((symbol . "emcp--server-build") (kind . "function"))
        (let* ((result (alist-get 'result response))
               (text (alist-get 'text (aref (alist-get 'content result) 0))))
          (should-not (alist-get 'isError result))
          (should (string-match-p
                   "(emcp--server-build :name[^\n]*\n\\.\\.\\."
                   text)))))))

(ert-deftest emcp-tests-find-references-not-found ()
  (emcp-tests-with-emcp-refs-scope
    (emcp-test-with-tool-response response 'emcp-tools-find-references
                                  '((symbol . "emcp--this-does-not-exist-at-all"))
      (let* ((result (alist-get 'result response))
             (text (alist-get 'text (aref (alist-get 'content result) 0))))
        (should (eq (alist-get 'isError result) t))
        (should (string-match-p "No symbol named" text))))))

(ert-deftest emcp-tests-find-references-no-matches ()
  ;; A bound symbol that nothing references.
  (let ((sym (make-symbol "emcp-tests--orphan-but-interned")))
    (intern (symbol-name sym))
    (emcp-tests-with-emcp-refs-scope
      (emcp-test-with-tool-response response 'emcp-tools-find-references
                                    `((symbol . ,(symbol-name sym)))
        (let* ((result (alist-get 'result response))
               (text (alist-get 'text (aref (alist-get 'content result) 0))))
          (should-not (alist-get 'isError result))
          (should (string-match-p "No references to" text)))))))

(ert-deftest emcp-tests-describe-function ()
  (emcp-test-with-tool-response response 'emcp-tools-describe
                                '((symbol . "emcp--server-destroy") (kind . "function"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (string-match-p "Release the resources of SERVER" text)))))

(ert-deftest emcp-tests-describe-any ()
  ;; emcp-log-level is both a variable and a constant
  (emcp-test-with-tool-response response 'emcp-tools-describe
                                '((symbol . "emcp-log-level"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (string-match-p "\\[variable\\]" text)))))

(defvar emcp-tests--var-target nil
  "Scratch variable used by the get/set-variable tool tests.")

(ert-deftest emcp-tests-get-variable ()
  (let ((emcp-tests--var-target '(1 "two" 3)))
    (emcp-test-with-tool-response response 'emcp-tools-get-variable
                                  '((name . "emcp-tests--var-target"))
      (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
        (should (equal text "(1 \"two\" 3)"))))))

(ert-deftest emcp-tests-get-variable-unbound ()
  (emcp-test-with-tool-response response 'emcp-tools-get-variable
                                '((name . "emcp-tests--definitely-not-a-variable"))
    (let* ((result (alist-get 'result response))
           (text (alist-get 'text (aref (alist-get 'content result) 0))))
      (should (eq (alist-get 'isError result) t))
      (should (string-match-p "not bound" text)))))

(ert-deftest emcp-tests-set-variable-literal ()
  (let ((emcp-tests--var-target nil))
    (emcp-test-with-tool-response response 'emcp-tools-set-variable
                                  '((name . "emcp-tests--var-target")
                                    (value . "(1 2 3)"))
      (should-not (alist-get 'isError (alist-get 'result response)))
      ;; `(1 2 3)' is stored as data, not evaluated
      (should (equal emcp-tests--var-target '(1 2 3))))))

(ert-deftest emcp-tests-set-variable-trailing-junk ()
  (let ((emcp-tests--var-target 'before))
    (emcp-test-with-tool-response response 'emcp-tools-set-variable
                                  '((name . "emcp-tests--var-target")
                                    (value . "42 extra"))
      (let* ((result (alist-get 'result response))
             (text (alist-get 'text (aref (alist-get 'content result) 0))))
        (should (eq (alist-get 'isError result) t))
        (should (string-match-p "Trailing" text))
        ;; The value must not have been set
        (should (eq emcp-tests--var-target 'before))))))

(ert-deftest emcp-tests-set-variable-malformed ()
  (let ((emcp-tests--var-target 'before))
    (emcp-test-with-tool-response response 'emcp-tools-set-variable
                                  '((name . "emcp-tests--var-target")
                                    (value . "(unbalanced"))
      (let ((result (alist-get 'result response)))
        (should (eq (alist-get 'isError result) t))
        (should (eq emcp-tests--var-target 'before))))))

(ert-deftest emcp-tests-set-variable-unbound ()
  (emcp-test-with-tool-response response 'emcp-tools-set-variable
                                '((name . "emcp-tests--never-defined-var")
                                  (value . "42"))
    (let ((result (alist-get 'result response)))
      (should (eq (alist-get 'isError result) t))
      ;; And the symbol must not have become bound as a side effect
      (should-not (boundp 'emcp-tests--never-defined-var)))))

(ert-deftest emcp-tests-describe-not-found ()
  (emcp-test-with-tool-response response 'emcp-tools-describe
                                '((symbol . "emcp--this-does-not-exist-at-all"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (string-match-p "No documentation found" text)))))

(ert-deftest emcp-tests-info-search ()
  (emcp-test-with-tool-response response 'emcp-tools-info-search
                                '((pattern . "defun") (manual . "elisp"))
    (let* ((content (alist-get 'content (alist-get 'result response)))
           (first (aref content 0)))
      (should (equal (alist-get 'type first) "resource_link"))
      (should (string-match-p "^info://" (alist-get 'uri first))))))

(provide 'emcp-tools-tests)
;;; emcp-tools-tests.el ends here
