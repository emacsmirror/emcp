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

(require 'emcp-confirm)
(require 'emcp-core)

(defgroup emcp-tools-send-keys ()
  "Gated key injection tool for EMCP."
  :group 'emcp)

(defcustom emcp-tools-send-keys-default-policy 'ask
  "Default action when no session mode applies.

t    always accept without prompting,
nil  always reject without prompting,
ask  open the confirmation buffer."
  :group 'emcp-tools-send-keys
  :type '(choice (const :tag "Always accept" t)
                 (const :tag "Always reject" nil)
                 (const :tag "Ask the user" ask)))

;;; Authorization

(defun emcp-tools-send-keys--authorize (session)
  "Authorize a send-keys call in SESSION.

Return t (accept), nil (reject), or `ask' (ask user)."
  (let ((mode (plist-get session :emcp-tools-send-keys-mode)))
    (cond
     ((eq mode 'accept) t)
     ((eq mode 'reject) nil)
     ((eq mode 'ask) 'ask)
     (t emcp-tools-send-keys-default-policy))))

;;; Confirmation buffer

(defun emcp-tools-send-keys--apply-action (action session)
  "Convert ACTION into a decision about a send-keys call in SESSION.

May mutate SESSION to record a session mode."
  (pcase action
    ('yes-once    t)
    ('no-once     nil)
    ('mode-accept (plist-put session :emcp-tools-send-keys-mode 'accept) t)
    ('mode-reject (plist-put session :emcp-tools-send-keys-mode 'reject) nil)))

(defun emcp-tools-send-keys--prompt (server session keys callback)
  "Open the confirmation buffer for SERVER, SESSION and KEYS.

CALLBACK is invoked with a t or nil decision after the user picks an
action.

Customize placement of the confirmation buffer by adding an entry for
`emcp-confirm-buffer-name' to `display-buffer-alist'."
  (emcp-confirm-prompt
   server session
   :title "send the keys"
   :body keys
   :on-dismiss 'no-once
   :groups
   `(( :title "Accept?"
       :actions ((?y "Yes" :result yes-once)
                 (?n "No"  :result no-once)))
     ( :title "Session mode (applies to all subsequent send-keys calls in this session)"
       :actions ((?a "Always accept" :result mode-accept)
                 (?r "Always reject" :result mode-reject))))
   :callback (lambda (action)
               (funcall callback
                        (emcp-tools-send-keys--apply-action action session)))))

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

Used only for logging.  The result is one of:
- `mode' if SESSION has an accept/reject mode set,
- `user' if SESSION has an `ask' mode set or
  `emcp-tools-send-keys-default-policy' is `ask' (the user is consulted via
  the confirmation buffer),
- `default' if `emcp-tools-send-keys-default-policy' is t or nil
  (auto-decided without asking)."
  (let ((mode (plist-get session :emcp-tools-send-keys-mode)))
    (cond
     ((memq mode '(accept reject)) 'mode)
     ((or (eq mode 'ask)
          (eq emcp-tools-send-keys-default-policy 'ask))
      'user)
     (t 'default))))

;;; The tool

(defun emcp-tools-send-keys--execute (keys target-window send-result)
  "Execute KEYS in TARGET-WINDOW.

SEND-RESULT receives the MCP tool result alist.  If TARGET-WINDOW is no
longer live, send an error result without executing anything."
  (cond
   ((not (window-live-p target-window))
    (funcall send-result
             `((content . [((type . "text")
                            (text . "Target window is no longer live."))])
               (isError . t))))
   (t
    (condition-case err
        (progn
          (with-selected-window target-window
            (execute-kbd-macro (kbd keys)))
          (funcall send-result
                   `((content . [((type . "text")
                                  (text . ,(format "Sent: %s" keys)))]))))
      (error
       (funcall send-result
                `((content . [((type . "text")
                               (text . ,(format "Error: %s"
                                                (error-message-string err))))])
                  (isError . t))))))))

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
  (let ((decision (emcp-tools-send-keys--authorize session))
        ;; Capture the source before the prompt opens: a mode-accept or mode-reject pick
        ;; by the user would otherwise relabel this call's source to `mode'.
        (source (emcp-tools-send-keys--decision-source session))
        ;; Capture now, since the confirmation buffer's `pop-to-buffer' may change the
        ;; selected window
        (target-window (selected-window)))
    (cl-flet ((maybe-execute (accept)
                (emcp--log server session
                  (info (emcp-tools-send-keys--format-log accept source keys)))
                (if accept
                    (emcp-tools-send-keys--execute keys target-window #'send-result)
                  (send-result
                   `((content . [((type . "text")
                                  (text . "User rejected send-keys."))])
                     (isError . t))))))
      (if (eq decision 'ask)
          (emcp-tools-send-keys--prompt server session keys #'maybe-execute)
        (maybe-execute decision)))))

(provide 'emcp-tools-send-keys)
;;; emcp-tools-send-keys.el ends here
