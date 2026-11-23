;;; emcp-confirm-tests.el --- Tests for the confirm buffer -*- lexical-binding: t -*-

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

;; Tests for the confirmation buffer in emcp-confirm.el.

;;; Code:

(require 'cl-lib)
(require 'emcp-confirm)
(require 'emcp-test-helpers)
(require 'ert)

(ert-deftest emcp-tests-confirm-result-action ()
  ;; Pressing a :result key invokes the callback once with that symbol and
  ;; kills the buffer.  Setting :on-dismiss also exercises the dispatch
  ;; path's `--pending'-clearing: if the kill-buffer-hook fired again, the
  ;; callback would be invoked twice.
  (let* ((calls nil)
         (cb (lambda (r) (push r calls)))
         (emcp-confirm-notify-function nil))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (let ((buf (emcp-confirm-prompt
                  nil '(:id "test-session")
                  :title "do thing"
                  :body "thing"
                  :on-dismiss 'no-once
                  :groups '((:title "Accept?"
                                    :actions ((?y "Yes" :result yes-once)
                                              (?n "No"  :result no-once))))
                  :callback cb)))
        (with-current-buffer buf (emcp-confirm--dispatch 'yes-once))
        (should (equal calls '(yes-once)))
        (should-not (buffer-live-p buf))))))

(ert-deftest emcp-tests-confirm-command-action-does-not-dismiss ()
  ;; A :command action runs the bound function without dismissing the buffer
  ;; or invoking the callback.
  (let* ((calls nil)
         (cmd-runs 0)
         (cmd (lambda () (interactive) (cl-incf cmd-runs)))
         (cb (lambda (r) (push r calls)))
         (emcp-confirm-notify-function nil))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (let ((buf (emcp-confirm-prompt
                  nil '(:id "test")
                  :title "do thing"
                  :body "thing"
                  :groups `((:actions ((?w "Aux" :command ,cmd))))
                  :callback cb)))
        (with-current-buffer buf
          (call-interactively (lookup-key (current-local-map) "w")))
        (should (= cmd-runs 1))
        (should (null calls))
        (should (buffer-live-p buf))
        (kill-buffer buf)))))

(ert-deftest emcp-tests-confirm-dismiss-invokes-callback ()
  ;; Killing the buffer without picking a :result action invokes the
  ;; callback with the :on-dismiss symbol.
  (let* ((calls nil)
         (cb (lambda (r) (push r calls)))
         (emcp-confirm-notify-function nil))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (let ((buf (emcp-confirm-prompt
                  nil '(:id "test")
                  :title "do thing"
                  :body ""
                  :on-dismiss 'no-once
                  :groups '((:actions ((?y "Yes" :result yes-once))))
                  :callback cb)))
        (kill-buffer buf)
        (should (equal calls '(no-once)))))))

(provide 'emcp-confirm-tests)
;;; emcp-confirm-tests.el ends here
