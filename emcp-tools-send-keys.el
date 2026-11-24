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
;; server-wide accept or reject mode.  Decisions are deliberately not cached:
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
  "Default action when no server mode applies.

t    always accept without prompting,
nil  always reject without prompting,
ask  open the confirmation buffer."
  :group 'emcp-tools-send-keys
  :type '(choice (const :tag "Always accept" t)
                 (const :tag "Always reject" nil)
                 (const :tag "Ask the user" ask)))

;;; Authorization

(defun emcp-tools-send-keys--authorize (server)
  "Authorize a send-keys call on SERVER.

Return t (accept), nil (reject), or `ask' (ask user)."
  (pcase (emcp--server-mode server :emcp-tools-send-keys-mode)
    ('accept t)
    ('reject nil)
    ('ask 'ask)
    (_ emcp-tools-send-keys-default-policy)))

;;; Confirmation buffer

(defun emcp-tools-send-keys--apply-action (action server)
  "Convert ACTION into a decision about a send-keys call on SERVER.

May record a server mode in SERVER."
  (pcase action
    ('yes-once    t)
    ('no-once     nil)
    ('cancel      nil)
    ('mode-accept
     (setf (emcp--server-mode server :emcp-tools-send-keys-mode) 'accept)
     t)
    ('mode-reject
     (setf (emcp--server-mode server :emcp-tools-send-keys-mode) 'reject)
     nil)))

(defun emcp-tools-send-keys--prompt (context keys callback)
  "Open the confirmation buffer for KEYS in the tool call CONTEXT.

CALLBACK is invoked with a t or nil decision and its reason, `user' if
the user picked an action or `cancel' if the client cancelled the
request.

Customize placement of the confirmation buffer by adding an entry for
`emcp-confirm-buffer-name' to `display-buffer-alist'."
  (emcp-confirm-prompt
   context
   :title "send the keys"
   :body keys
   :on-dismiss 'no-once
   :on-cancel 'cancel
   :groups
   `(( :title "Accept?"
       :actions ((?y "Yes" :result yes-once)
                 (?n "No"  :result no-once)))
     ( :title "Server mode (applies to all subsequent send-keys calls on this server)"
       :actions ((?a "Always accept" :result mode-accept)
                 (?r "Always reject" :result mode-reject))))
   :callback (lambda (action)
               (funcall callback
                        (emcp-tools-send-keys--apply-action
                         action (emcp-context-server context))
                        (if (eq action 'cancel) 'cancel 'user)))))

;;; Decision logging

(defun emcp-tools-send-keys--format-log (decision reason keys)
  "Format a single decision-log line.

DECISION is t (accept) or nil (reject).  REASON is a symbol describing
why this decision was made: `user', `mode', `default' or `cancel' if the
client cancelled the request.  KEYS is the key sequence string that was
decided on."
  (format "send-keys %s %s %s"
          (if decision "ACCEPT" "REJECT")
          (symbol-name reason)
          (if (> (length keys) 80) (concat (substring keys 0 77) "...") keys)))

(defun emcp-tools-send-keys--decision-source (server)
  "Return why a send-keys call on SERVER was decided without asking the user.

Used only for logging.  The result is `mode' if SERVER has an
accept/reject mode set and `default' if the decision comes from
`emcp-tools-send-keys-default-policy'."
  (if (memq (emcp--server-mode server :emcp-tools-send-keys-mode) '(accept reject))
      'mode
    'default))

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

Each call requires user confirmation, with optional accept or reject
modes for the whole server.  Decisions are not cached across calls: the
effect of a key sequence depends on the current buffer, mode, and
minibuffer state, so a past decision is rarely a good guide for a future
one."
  :name "send-keys"
  :description "Send a key sequence to Emacs as if the user typed it.

Each call is gated by user confirmation and may block until the user
responds.  Decisions are not cached, so every call prompts the user.

Prefer a more specialized tool if available to minimize user interaction
and decision fatigue."
  :async t
  (let ((decision (emcp-tools-send-keys--authorize server))
        ;; Capture now, since the confirmation buffer's
        ;; `pop-to-buffer' may change the selected window
        (target-window (selected-window)))
    (cl-flet ((maybe-execute (accept reason)
                (emcp--log server context
                  (info (emcp-tools-send-keys--format-log accept reason keys)))
                (if accept
                    (emcp-tools-send-keys--execute keys target-window #'send-result)
                  (send-result
                   `((content . [((type . "text")
                                  (text . "User rejected send-keys."))])
                     (isError . t))))))
      (if (eq decision 'ask)
          (emcp-tools-send-keys--prompt context keys #'maybe-execute)
        (maybe-execute decision (emcp-tools-send-keys--decision-source server))))))

(provide 'emcp-tools-send-keys)
;;; emcp-tools-send-keys.el ends here
