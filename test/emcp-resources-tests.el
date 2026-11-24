;;; emcp-resources-tests.el --- Tests for the built-in resources -*- lexical-binding: t -*-

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

;; Tests for the info resources in emcp-resources.el.

;;; Code:

(require 'emcp-resources)
(require 'emcp-test-helpers)
(require 'ert)

(ert-deftest emcp-tests-resource-template-list ()
  (emcp-test-with-server server (emcp--server-build :resources '(emcp-resource-info-node))
    (let* ((client (emcp-test-make-client :server server))
           (response (emcp-test-client-request client "resources/templates/list"))
           (templates (alist-get 'resourceTemplates (emcp-test-response-data response))))
      (should (= (length templates) 1))
      (should (equal (alist-get 'uriTemplate (aref templates 0))
                     "info://{manual}/{node}")))))

(ert-deftest emcp-tests-resource-read-info ()
  (emcp-test-with-server server (emcp--server-build :resources '(emcp-resource-info-node))
    (let* ((client (emcp-test-make-client :server server))
           (response (emcp-test-client-request client "resources/read"
                                               '((uri . "info://elisp/Top"))))
           (content (aref (alist-get 'contents (emcp-test-response-data response)) 0))
           (text (alist-get 'text content)))
      (should (equal (alist-get 'mimeType content) "text/plain"))
      (should (string-match-p "Emacs Lisp" text))
      (should (string-search "\nMenu:\n  Introduction: info://elisp/Introduction\n" text))
      (should (string-search "  Lisp Data Types: info://elisp/Lisp%20Data%20Types\n" text)))))

(ert-deftest emcp-tests-resource-read-info-not-found ()
  (emcp-test-with-server server (emcp--server-build :resources '(emcp-resource-info-node))
    (let ((client (emcp-test-make-client :server server)))
      (dolist (uri '("info://elisp/No%20Such%20Node" "info://no-such-manual/Top"))
        (ert-info (uri)
          (let ((error (alist-get 'error (emcp-test-client-request client "resources/read"
                                                                   `((uri . ,uri))))))
            (should (= (alist-get 'code error) emcp--jsonrpc-invalid-params))
            (should (equal (alist-get 'data error) `((uri . ,uri))))))))))

(ert-deftest emcp-tests-info-replace-xrefs ()
  ;; *note Node::
  (should (equal (emcp-resources--info-replace-xrefs "*note Symbols::" "elisp")
                 "Symbols (info://elisp/Symbols)"))
  ;; *note Label: Node.
  (should (equal (emcp-resources--info-replace-xrefs "*note my label: Symbols." "elisp")
                 "my label (info://elisp/Symbols)"))
  ;; *note (manual)Node::
  (should (equal (emcp-resources--info-replace-xrefs "*note (emacs)Buffers::" "elisp")
                 "Buffers (info://emacs/Buffers)"))
  ;; *note Label: (manual)Node.
  (should (equal (emcp-resources--info-replace-xrefs "*note see this: (emacs)Buffers." "elisp")
                 "see this (info://emacs/Buffers)")))

(provide 'emcp-resources-tests)
;;; emcp-resources-tests.el ends here
