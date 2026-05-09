;;; emcp-tools-send-keys.el --- Gated key injection tool for EMCP -*- lexical-binding: t -*-

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

;; An MCP tool to allow agents to send arbitrary key sequences to Emacs.  Each
;; call goes through a confirmation buffer; the user can also enable a
;; session-wide accept or reject mode.  Decisions are deliberately not cached:
;; identical key sequences may have very different effects depending on the
;; current buffer, mode, and minibuffer state, so reusing a past decision is
;; rarely safe.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'emcp-core)

(defgroup emcp-tools-send-keys ()
  "Gated key injection tool for EMCP."
  :group 'emcp)

(defcustom emcp-tools-send-keys-default-policy 'query
  "Default action when no session mode applies.

t      always accept without prompting,
nil    always reject without prompting,
query  open the confirmation buffer."
  :group 'emcp-tools-send-keys
  :type '(choice (const :tag "Always accept" t)
                 (const :tag "Always reject" nil)
                 (const :tag "Ask the user" query)))

(defcustom emcp-tools-send-keys-confirm-buffer-name "*EMCP send-keys*"
  "Buffer name for the send-keys confirmation UI."
  :group 'emcp-tools-send-keys
  :type 'string)

;;; Authorization

(defun emcp-tools-send-keys--authorize (session)
  "Authorize a send-keys call in SESSION.

Return t (accept), nil (reject), or `prompt' (ask user)."
  (let ((mode (plist-get session :emcp-tools-send-keys-mode)))
    (cond
     ((eq mode 'accept) t)
     ((eq mode 'reject) nil)
     ((eq emcp-tools-send-keys-default-policy 'query) 'prompt)
     (t emcp-tools-send-keys-default-policy))))

;;; Confirmation buffer

(defvar-local emcp-tools-send-keys--pending nil
  "Plist of the buffer's pending request.

Keys: :session SESSION :keys KEYS :callback CB.")

(defvar emcp-tools-send-keys-confirm-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "y" (lambda () (interactive) (emcp-tools-send-keys--dispatch 'yes-once)))
    (define-key m "n" (lambda () (interactive) (emcp-tools-send-keys--dispatch 'no-once)))
    (define-key m "a" (lambda () (interactive) (emcp-tools-send-keys--dispatch 'mode-accept)))
    (define-key m "r" (lambda () (interactive) (emcp-tools-send-keys--dispatch 'mode-reject)))
    m)
  "Keymap for `emcp-tools-send-keys-confirm-mode'.")

(define-derived-mode emcp-tools-send-keys-confirm-mode special-mode "EMCP-keys"
  "Major mode for the EMCP send-keys confirmation buffer."
  (setq buffer-read-only t))

(defun emcp-tools-send-keys--dispatch (action)
  "Apply ACTION for the buffer's pending request and dismiss the buffer."
  (let* ((pending emcp-tools-send-keys--pending)
         (session (plist-get pending :session))
         (callback (plist-get pending :callback)))
    (unless pending (user-error "No pending send-keys request in this buffer"))
    (funcall callback (emcp-tools-send-keys--apply-action action session))
    (quit-window t)))

(defun emcp-tools-send-keys--apply-action (action session)
  "Convert ACTION into a decision about a send-keys call in SESSION.

May mutate SESSION to record a session mode."
  (pcase action
    ('yes-once    t)
    ('no-once     nil)
    ('mode-accept (plist-put session :emcp-tools-send-keys-mode 'accept) t)
    ('mode-reject (plist-put session :emcp-tools-send-keys-mode 'reject) nil)))

(defun emcp-tools-send-keys--render (session keys)
  "Render the confirmation buffer body for SESSION and KEYS."
  (let ((session-id (or (plist-get session :id) "?")))
    (concat
     (format "The agent in session %s wants to send the keys:\n\n"
             (substring session-id 0 (min 8 (length session-id))))
     (format "  %s\n" keys)
     "\nAccept?\n"
     "  [y] Yes      [n] No\n"
     "\nSession mode (applies to all subsequent send-keys calls in this session):\n"
     "  [a] Always accept     [r] Always reject\n")))

(defun emcp-tools-send-keys--prompt (session keys callback)
  "Open the confirmation buffer for SESSION and KEYS.

CALLBACK is invoked with a t or nil decision after the user picks an
action.

Customize placement of the confirmation buffer by adding an entry for
`emcp-tools-send-keys-confirm-buffer-name' to `display-buffer-alist'."
  (let ((buf (get-buffer-create emcp-tools-send-keys-confirm-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (emcp-tools-send-keys-confirm-mode)
        (insert (emcp-tools-send-keys--render session keys)))
      (setq emcp-tools-send-keys--pending
            (list :session session :keys keys :callback callback)))
    (pop-to-buffer buf '((display-buffer-in-side-window)
                         (side . bottom)
                         (window-height . 0.4)))))

;;; Decision logging

(defun emcp-tools-send-keys--format-log (decision reason keys)
  "Format a single decision-log line.

DECISION is t (accept) or nil (reject).  REASON is a symbol describing
why this decision was made.  KEYS is the key sequence string that was
decided on."
  (format "send-keys %s %s %s"
          (if decision "ACCEPT" "REJECT")
          (symbol-name reason)
          (if (> (length keys) 80) (concat (substring keys 0 77) "...") keys)))

(defun emcp-tools-send-keys--decision-source (session)
  "Return a symbol describing why a send-keys call was decided in SESSION.

Used only for logging.  The result is one of `mode' or `default'."
  (let ((mode (plist-get session :emcp-tools-send-keys-mode)))
    (if (memq mode '(accept reject)) 'mode 'default)))

;;; The tool

(defun emcp-tools-send-keys--execute (keys send-result)
  "Execute KEYS and call SEND-RESULT with the MCP tool result alist."
  (condition-case err
      (progn
        (execute-kbd-macro (kbd keys))
        (funcall send-result
                 `((content . [((type . "text")
                                (text . ,(format "Sent: %s" keys)))]))))
    (error
     (funcall send-result
              `((content . [((type . "text")
                             (text . ,(format "Error: %s"
                                              (error-message-string err))))])
                (isError . t))))))

(emcp-deftool emcp-tools-send-keys
    ((keys "Key sequence in `kbd' notation, e.g. =C-x C-f=, =M-x list-buffers RET=, or =h e l l o= for literal characters."))
  "Send a key sequence to Emacs as if the user typed it.

Each call requires user confirmation, with optional session-wide accept
or reject modes for trusted or untrusted sessions.  Decisions are not
cached across calls: the effect of a key sequence depends on the current
buffer, mode, and minibuffer state, so a past decision is rarely a good
guide for a future one."
  :name "send-keys"
  :description "Send a key sequence to Emacs as if the user typed it.

Each call is gated by user confirmation and may block until the user
responds.  Decisions are not cached, so every call prompts the user.

Prefer a more specialized tool if available to minimize user interaction
and decision fatigue."
  :async t
  (let ((decision (emcp-tools-send-keys--authorize session)))
    (cl-flet ((maybe-execute (accept reason)
                (emcp--log server session
                  (info (emcp-tools-send-keys--format-log accept reason keys)))
                (if accept
                    (emcp-tools-send-keys--execute keys #'send-result)
                  (send-result
                   `((content . [((type . "text")
                                  (text . "User rejected send-keys."))])
                     (isError . t))))))
      (pcase decision
        ('prompt
         (emcp-tools-send-keys--prompt
          session keys
          (lambda (d) (maybe-execute d 'user))))
        (_
         (maybe-execute decision (emcp-tools-send-keys--decision-source session)))))))

(provide 'emcp-tools-send-keys)
;;; emcp-tools-send-keys.el ends here
