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

(provide 'emcp-uri-tests)
;;; emcp-uri-tests.el ends here
