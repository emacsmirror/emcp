;;; emcp-tests.el --- Tests for emcp.el -*- lexical-binding: t -*-

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

;; Tests for profile handling in emcp.el.

;;; Code:

(require 'emcp)
(require 'emcp-test-helpers)
(require 'ert)

(defun emcp-tests--make-tool (name)
  "Define a tool with MCP name NAME on a fresh uninterned symbol and return it."
  (let ((sym (make-symbol name)))
    (eval `(emcp-deftool ,sym ()
             "A test tool."
             :name ,name
             '((content . []))))
    sym))

(ert-deftest emcp-tests-resolve-profile-include-cycle ()
  ;; Includes are followed recursively and a cycle does not recurse forever
  (let* ((a (emcp-tests--make-tool "a"))
         (b (emcp-tests--make-tool "b"))
         (c (emcp-tests--make-tool "c"))
         (emcp-profiles `((pa . (:include (pb) :tools (,a)))
                          (pb . (:include (pa pc) :tools (,b)))
                          (pc . (:tools (,c)))))
         (resolved (emcp--resolve-profile (alist-get 'pa emcp-profiles))))
    (should-not (plist-member resolved :include))
    (should (equal (sort (mapcar #'symbol-name (plist-get resolved :tools)) #'string<)
                   '("a" "b" "c")))))

(ert-deftest emcp-tests-resolve-profile-last-definition-wins ()
  (let* ((first (emcp-tests--make-tool "same"))
         (last (emcp-tests--make-tool "same"))
         (emcp-profiles `((p . (:tools (,first ,last))))))
    (should (equal (plist-get (emcp--resolve-profile (alist-get 'p emcp-profiles)) :tools)
                   (list last)))))

(ert-deftest emcp-tests-resolve-profile-unknown-include ()
  (let ((emcp-profiles '((p . (:include (does-not-exist))))))
    (should-error (emcp--resolve-profile (alist-get 'p emcp-profiles))
                  :type 'user-error)))

(provide 'emcp-tests)
;;; emcp-tests.el ends here
