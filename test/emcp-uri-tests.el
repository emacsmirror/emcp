;;; emcp-uri-tests.el --- Tests for URI templates -*- lexical-binding: t -*-

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

;; Tests for the URI template utilities in emcp-uri.el.

;;; Code:

(require 'emcp-uri)
(require 'ert)

(ert-deftest emcp-tests-uri-template-match ()
  (let ((compiled (emcp-uri--compile-template "info://{manual}/{node}")))
    (should (equal (emcp-uri--match "info://elisp/Symbols" compiled)
                   '((manual . "elisp") (node . "Symbols"))))
    (should (equal (emcp-uri--match "info://elisp/Buffer%20List" compiled)
                   '((manual . "elisp") (node . "Buffer List"))))
    (should-not (emcp-uri--match "http://example.com" compiled))))

(ert-deftest emcp-tests-uri-build ()
  (should (equal (emcp-uri--build "info://{manual}/{node}"
                                  '((manual . "elisp") (node . "Buffer List")))
                 "info://elisp/Buffer%20List")))

(ert-deftest emcp-tests-uri-reserved-match ()
  "{+path} matches multiple segments and binds `path'."
  (let ((compiled (emcp-uri--compile-template "file://{+path}")))
    (should (equal (emcp-uri--match "file:///home/user/My%20Notes.org" compiled)
                   '((path . "/home/user/My Notes.org"))))))

(ert-deftest emcp-tests-uri-simple-match-single-segment ()
  "{var} only matches a single path segment."
  (should-not (emcp-uri--match "info://elisp/a/b"
                               (emcp-uri--compile-template "info://{manual}/{node}"))))

(ert-deftest emcp-tests-uri-reserved-build ()
  "{+var} keeps reserved characters and percent-encoded triplets.

It encodes everything else, including a % that starts no triplet."
  (should (equal (emcp-uri--build "file://{+path}" '((path . "/home/My Notes.org")))
                 "file:///home/My%20Notes.org"))
  (should (equal (emcp-uri--build "file://{+path}" '((path . "/a%20b/100%/ä")))
                 "file:///a%20b/100%25/%C3%A4")))

(provide 'emcp-uri-tests)
;;; emcp-uri-tests.el ends here
