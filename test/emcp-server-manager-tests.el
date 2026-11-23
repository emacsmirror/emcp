;;; emcp-server-manager-tests.el --- Tests for the server manager -*- lexical-binding: t -*-

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

;; Tests for the server manager UI in emcp-server-manager.el.

;;; Code:

(require 'emcp-test-helpers)
(require 'emcp-server-manager)
(require 'ert)

(defmacro emcp-tests-with-server-manager (profiles &rest body)
  "Start servers for PROFILES and run BODY in the server manager buffer.

Stop all servers and kill the buffer afterwards."
  (declare (indent 1))
  (let ((profiles-var (gensym "profiles")))
    `(let* ((,profiles-var ,profiles)
            (emcp-profiles (mapcar #'list ,profiles-var))
            (emcp--servers nil)
            (emcp-http-port t)
            (emcp-tools-eval-default-policy 'ask)
            (emcp-tools-send-keys-default-policy 'ask))
       (unwind-protect
           (progn
             (mapc #'emcp-start ,profiles-var)
             (save-window-excursion (emcp-server-manager))
             (with-current-buffer emcp-server-manager-buffer-name
               ,@body))
         ;; Stop every server even if stopping one of them fails
         (dolist (entry emcp--servers)
           (ignore-errors (emcp-stop (car entry))))
         (when-let* ((buf (get-buffer emcp-server-manager-buffer-name)))
           (kill-buffer buf))))))

(defun emcp-tests-server-manager-goto (profile)
  "Move point to the heading of PROFILE's server section."
  (goto-char (point-min))
  (search-forward (format "[%s]" profile))
  (beginning-of-line))

(defun emcp-tests-server-manager-active-mode (profile label)
  "Return the highlighted mode on the LABEL line of PROFILE's section."
  (save-excursion
    (emcp-tests-server-manager-goto profile)
    (search-forward (concat label ":"))
    (when-let* ((match (text-property-search-forward
                        'font-lock-face 'emcp-server-manager-mode-active t))
                ((<= (prop-match-end match) (line-end-position))))
      (buffer-substring-no-properties (prop-match-beginning match)
                                      (prop-match-end match)))))

(defun emcp-tests-server-manager-press (key)
  "Run the command bound to KEY in the current buffer."
  (call-interactively (lookup-key (current-local-map) (kbd key))))

(ert-deftest emcp-tests-server-manager-renders-servers ()
  (emcp-tests-with-server-manager '(alpha beta)
    (setf (emcp--server-mode (alist-get 'beta emcp--servers) :emcp-tools-eval-mode)
          'reject)
    (emcp-server-manager-refresh)
    (dolist (entry emcp--servers)
      (should (string-search (format "[%s]  %s" (car entry) (emcp-server-url (cdr entry)))
                             (buffer-string))))
    (should (equal (emcp-tests-server-manager-active-mode 'alpha "Eval") "ask"))
    (should (equal (emcp-tests-server-manager-active-mode 'alpha "Send-keys") "ask"))
    (should (equal (emcp-tests-server-manager-active-mode 'beta "Eval") "reject"))
    (should (equal (emcp-tests-server-manager-active-mode 'beta "Send-keys") "ask"))))

(ert-deftest emcp-tests-server-manager-cycle-modes ()
  (emcp-tests-with-server-manager '(alpha beta)
    (let ((server (alist-get 'beta emcp--servers)))
      (emcp-tests-server-manager-goto 'beta)
      (emcp-tests-server-manager-press "e")
      (should (eq (emcp--server-mode server :emcp-tools-eval-mode) 'accept))
      (should (equal (emcp-tests-server-manager-active-mode 'beta "Eval") "accept"))
      (emcp-tests-server-manager-press "e")
      (should (eq (emcp--server-mode server :emcp-tools-eval-mode) 'reject))
      (should (equal (emcp-tests-server-manager-active-mode 'beta "Eval") "reject"))
      (emcp-tests-server-manager-press "K")
      (should (eq (emcp--server-mode server :emcp-tools-send-keys-mode) 'accept))
      (should (equal (emcp-tests-server-manager-active-mode 'beta "Send-keys")
                     "accept")))))

(ert-deftest emcp-tests-server-manager-kill-server ()
  (emcp-tests-with-server-manager '(alpha beta)
    (emcp-tests-server-manager-goto 'alpha)
    (forward-line 2)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (emcp-tests-server-manager-press "k"))
    (should (equal (mapcar #'car emcp--servers) '(beta)))))

(provide 'emcp-server-manager-tests)
;;; emcp-server-manager-tests.el ends here
