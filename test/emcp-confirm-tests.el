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

(defmacro emcp-tests-with-confirm-context (context &rest body)
  "Bind CONTEXT to a tool call context of a test server and execute BODY.

The client is called \"Test Client\" and the server \"emcp-test\".
`pop-to-buffer' is stubbed out and desktop notifications are disabled.
Confirmation buffers that BODY leaves behind are killed afterwards."
  (declare (indent 1) (debug (symbolp body)))
  (let ((server (gensym "server"))
        (buffers (gensym "buffers")))
    `(emcp-test-with-server ,server (emcp--server-build :name "emcp-test")
       (let ((,context (emcp--make-context
                        :server ,server
                        :client-info (emcp-test-to-hash-table
                                      '((name . "test-client") (title . "Test Client")))))
             (emcp-confirm-notify-function nil)
             (,buffers (buffer-list)))
         (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
           (unwind-protect
               (progn ,@body)
             (dolist (buf (cl-set-difference (buffer-list) ,buffers))
               (when (eq (buffer-local-value 'major-mode buf) 'emcp-confirm-mode)
                 (kill-buffer buf)))))))))

(ert-deftest emcp-tests-confirm-header-shows-context-label ()
  (emcp-tests-with-confirm-context context
    (let ((buf (emcp-confirm-prompt
                context
                :title "do thing"
                :body "thing"
                :groups '((:actions ((?y "Yes" :result yes-once))))
                :callback #'ignore)))
      (with-current-buffer buf
        (should (string-prefix-p "Test Client (emcp-test) wants to do thing:"
                                 (buffer-string)))))))

(ert-deftest emcp-tests-confirm-result-action ()
  ;; Pressing a :result key invokes the callback once with that symbol and
  ;; kills the buffer.  Setting :on-dismiss also exercises the dispatch
  ;; path's `emcp-confirm--pending'-clearing: if the kill-buffer-hook fired
  ;; again, the callback would be invoked twice.
  (emcp-tests-with-confirm-context context
    (let* ((calls nil)
           (buf (emcp-confirm-prompt
                 context
                 :title "do thing"
                 :body "thing"
                 :on-dismiss 'no-once
                 :groups '((:title "Accept?"
                                   :actions ((?y "Yes" :result yes-once)
                                             (?n "No"  :result no-once))))
                 :callback (lambda (r) (push r calls)))))
      (with-current-buffer buf (emcp-confirm--dispatch 'yes-once))
      (should (equal calls '(yes-once)))
      (should-not (buffer-live-p buf)))))

(ert-deftest emcp-tests-confirm-command-action-does-not-dismiss ()
  ;; A :command action runs the bound function with access to :data, without
  ;; dismissing the buffer or invoking the callback.
  (emcp-tests-with-confirm-context context
    (let* ((calls nil)
           (seen-data nil)
           (cmd (lambda ()
                  (interactive)
                  (push (plist-get emcp-confirm--pending :data) seen-data)))
           (buf (emcp-confirm-prompt
                 context
                 :title "do thing"
                 :body "thing"
                 :data '(:code "(+ 1 2)")
                 :groups `((:actions ((?w "Aux" :command ,cmd))))
                 :callback (lambda (r) (push r calls)))))
      (with-current-buffer buf
        (call-interactively (lookup-key (current-local-map) "w")))
      (should (equal seen-data '((:code "(+ 1 2)"))))
      (should (null calls))
      (should (buffer-live-p buf)))))

(ert-deftest emcp-tests-confirm-dismiss-invokes-callback ()
  ;; Killing the buffer without picking a :result action invokes the
  ;; callback with the :on-dismiss symbol.
  (emcp-tests-with-confirm-context context
    (let* ((calls nil)
           (buf (emcp-confirm-prompt
                 context
                 :title "do thing"
                 :body ""
                 :on-dismiss 'no-once
                 :groups '((:actions ((?y "Yes" :result yes-once))))
                 :callback (lambda (r) (push r calls)))))
      (kill-buffer buf)
      (should (equal calls '(no-once))))))

(ert-deftest emcp-tests-confirm-cancel-dismisses ()
  "Cancelling the request kills the buffer.

The callback receives the :on-cancel value."
  (emcp-tests-with-confirm-context context
    (let* ((calls nil)
           (buf (emcp-confirm-prompt
                 context
                 :title "do thing"
                 :body ""
                 :on-dismiss 'no-once
                 :on-cancel 'cancel
                 :groups '((:actions ((?y "Yes" :result yes-once))))
                 :callback (lambda (r) (push r calls)))))
      (emcp--context-cancel context)
      (should-not (buffer-live-p buf))
      (should (equal calls '(cancel))))))

(provide 'emcp-confirm-tests)
;;; emcp-confirm-tests.el ends here
