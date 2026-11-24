;;; emcp-tools-send-keys-tests.el --- Tests for the send-keys tool -*- lexical-binding: t -*-

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

;; Tests for the send-keys tool in emcp-tools-send-keys.el.

;;; Code:

(require 'emcp-test-helpers)
(require 'emcp-tools-send-keys)
(require 'ert)

;;; Authorization

(ert-deftest emcp-tests-send-keys-authorize-mode-accept ()
  ;; Server mode `accept' overrides a default-reject policy.
  (emcp-test-with-server server (emcp--server-build)
    (setf (emcp--server-mode server :emcp-tools-send-keys-mode) 'accept)
    (let ((emcp-tools-send-keys-default-policy nil))
      (should (eq (emcp-tools-send-keys--authorize server) t)))))

(ert-deftest emcp-tests-send-keys-authorize-mode-reject ()
  ;; Server mode `reject' overrides a default-accept policy.
  (emcp-test-with-server server (emcp--server-build)
    (setf (emcp--server-mode server :emcp-tools-send-keys-mode) 'reject)
    (let ((emcp-tools-send-keys-default-policy t))
      (should (eq (emcp-tools-send-keys--authorize server) nil)))))

(ert-deftest emcp-tests-send-keys-authorize-default-policy ()
  (emcp-test-with-server server (emcp--server-build)
    (let ((emcp-tools-send-keys-default-policy t))
      (should (eq (emcp-tools-send-keys--authorize server) t)))
    (let ((emcp-tools-send-keys-default-policy nil))
      (should (eq (emcp-tools-send-keys--authorize server) nil)))
    (let ((emcp-tools-send-keys-default-policy 'ask))
      (should (eq (emcp-tools-send-keys--authorize server) 'ask)))))

(ert-deftest emcp-tests-send-keys-action-once ()
  (emcp-test-with-server server (emcp--server-build)
    (should (eq (emcp-tools-send-keys--apply-action 'yes-once server) t))
    (should (eq (emcp-tools-send-keys--apply-action 'no-once server) nil))))

(ert-deftest emcp-tests-send-keys-action-mode ()
  (emcp-test-with-server server (emcp--server-build)
    (let ((emcp-tools-send-keys-default-policy 'ask))
      (should (eq (emcp-tools-send-keys--apply-action 'mode-accept server) t))
      (should (eq (emcp-tools-send-keys--authorize server) t))
      (should (eq (emcp-tools-send-keys--apply-action 'mode-reject server) nil))
      (should (eq (emcp-tools-send-keys--authorize server) nil)))))

;;; The tool

(ert-deftest emcp-tests-send-keys-tool-default-accept ()
  ;; Self-insert keys land in the buffer of the selected window at call time.
  (let ((emcp-tools-send-keys-default-policy t)
        (buf (generate-new-buffer " *emcp-tests-send-keys-accept*")))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buf)
          (emcp-test-with-tool-response response 'emcp-tools-send-keys
                                        '((keys . "h e l l o"))
            (should-not (alist-get 'isError
                                   (alist-get 'result response))))
          (with-current-buffer buf
            (should (equal (buffer-string) "hello"))))
      (kill-buffer buf))))

(ert-deftest emcp-tests-send-keys-tool-default-reject ()
  (let ((emcp-tools-send-keys-default-policy nil)
        (buf (generate-new-buffer " *emcp-tests-send-keys-reject*")))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buf)
          (emcp-test-with-tool-response response 'emcp-tools-send-keys
                                        '((keys . "h e l l o"))
            (should (eq (alist-get 'isError
                                   (alist-get 'result response))
                        t)))
          ;; Buffer must remain untouched on rejection
          (with-current-buffer buf
            (should (equal (buffer-string) ""))))
      (kill-buffer buf))))

(ert-deftest emcp-tests-send-keys-execute-window-dead ()
  ;; A dead target window yields an error result without executing keys.
  (let ((result nil)
        (win (split-window)))
    (delete-window win)
    (emcp-tools-send-keys--execute "h e l l o" win
                                   (lambda (r) (setq result r)))
    (should (eq (alist-get 'isError result) t))))

(provide 'emcp-tools-send-keys-tests)
;;; emcp-tools-send-keys-tests.el ends here
