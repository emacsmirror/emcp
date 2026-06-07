;;; emcp-tools-eval.el --- Gated code evaluation tool for EMCP -*- lexical-binding: t -*-

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

;; An MCP tool to allow agents to evaluate arbitrary Emacs Lisp.  Each call goes through a
;; confirmation buffer; the user can also enable a session-wide accept or reject mode.

;;; Code:

(require 'cl-lib)
(require 'pp)
(require 'subr-x)

(require 'emcp-confirm)
(require 'emcp-core)

(define-error 'emcp-tools-eval-parse-error
              "Failed to parse Emacs Lisp source"
              'emcp-error)

(defgroup emcp-tools-eval ()
  "Gated Emacs Lisp evaluation tool for EMCP."
  :group 'emcp)

(defcustom emcp-tools-eval-default-policy 'ask
  "Default action when no session mode applies.

t    always accept without prompting,
nil  always reject without prompting,
ask  open the confirmation buffer."
  :group 'emcp-tools-eval
  :type '(choice (const :tag "Always accept" t)
                 (const :tag "Always reject" nil)
                 (const :tag "Ask the user" ask)))

;;; Parsing

(defun emcp-tools-eval--parse (code)
  "Parse CODE into a single canonical form.

Returns the form on success, signals `emcp-tools-eval-parse-error' with
a message on failure.  Multiple top-level forms are wrapped in (progn
...)."
  (let ((forms ()) (pos 0) (len (length code)))
    (condition-case err
        (cl-block done
          (while (< pos len)
            (when (string-match-p "\\`[[:space:]]*\\'" (substring code pos))
              (cl-return-from done))
            (pcase-let ((`(,form . ,new-pos) (read-from-string code pos)))
              (push form forms)
              (setq pos new-pos))))
      (end-of-file
       (signal 'emcp-tools-eval-parse-error
               (list (format "Unbalanced or incomplete input: %s"
                             (error-message-string err)))))
      (invalid-read-syntax
       (signal 'emcp-tools-eval-parse-error
               (list (format "Invalid Lisp syntax: %s"
                             (error-message-string err))))))
    (pcase (nreverse forms)
      ('()           (signal 'emcp-tools-eval-parse-error
                             '("No forms in input")))
      (`(,single)    single)
      (forms         `(progn ,@forms)))))

;;; Authorization

(defun emcp-tools-eval--authorize (session)
  "Authorize an eval call in SESSION.

Return t (accept), nil (reject), or `ask' (ask user)."
  (let ((mode (plist-get session :emcp-tools-eval-mode)))
    (cond
     ((eq mode 'accept) t)
     ((eq mode 'reject) nil)
     ((eq mode 'ask) 'ask)
     (t emcp-tools-eval-default-policy))))

;;; Confirmation buffer

(defun emcp-tools-eval--copy-code-to-kill-ring ()
  "Copy the code under review to the `kill-ring' without closing the buffer."
  (interactive)
  (kill-new (plist-get (plist-get emcp-confirm--pending :context) :code))
  (message "EMCP: code copied to kill-ring"))

(defun emcp-tools-eval--apply-action (action session)
  "Convert ACTION into a decision about an eval call in SESSION.

May mutate SESSION to record a session mode."
  (pcase action
    ('yes-once    t)
    ('no-once     nil)
    ('mode-accept (plist-put session :emcp-tools-eval-mode 'accept) t)
    ('mode-reject (plist-put session :emcp-tools-eval-mode 'reject) nil)))

(defun emcp-tools-eval--fontify-elisp (str)
  "Return STR with `emacs-lisp-mode' font-lock applied.

Uses a temp buffer in `emacs-lisp-mode' and `font-lock-ensure' so that
the returned string carries text properties that the confirmation
buffer (in `emcp-confirm-mode', a `special-mode' derivative without
font-lock of its own) renders directly."
  (with-temp-buffer
    (insert str)
    (delay-mode-hooks (emacs-lisp-mode))
    (font-lock-ensure)
    (buffer-string)))

(defun emcp-tools-eval--prompt (server session form callback)
  "Open the confirmation buffer for SERVER, SESSION and FORM.

CALLBACK is invoked with a t or nil decision after the user picks an
action."
  (let* ((pretty (string-trim (pp-to-string form)))
         (code (emcp-tools-eval--fontify-elisp pretty)))
    (emcp-confirm-prompt
     server session
     :title "evaluate"
     :body code
     :context (list :code code)
     :on-dismiss 'no-once
     :groups
     `(( :title "Accept?"
         :actions ((?y "Yes" :result yes-once)
                   (?n "No"  :result no-once)))
       ( :title "Session mode (applies to all subsequent eval calls in this session)"
         :actions ((?a "Always accept" :result mode-accept)
                   (?r "Always reject" :result mode-reject)))
       ( :actions ((?w "Copy code to kill-ring"
                       :command emcp-tools-eval--copy-code-to-kill-ring))))
     :callback (lambda (action)
                 (funcall callback
                          (emcp-tools-eval--apply-action action session))))))

;;; Decision logging

(defun emcp-tools-eval--format-log (decision reason form)
  "Format a single decision-log line.

DECISION is t (accept) or nil (reject).  REASON is a symbol describing
why this decision was made (e.g. `user', `mode', `default').  FORM is
the form that was decided on."
  (let ((pp (string-trim (pp-to-string form))))
    (format "eval %s %s %s"
            (if decision "ACCEPT" "REJECT")
            (symbol-name reason)
            (if (> (length pp) 80) (concat (substring pp 0 77) "...") pp))))

;;; The tool

(defun emcp-tools-eval--decision-source (session)
  "Return a symbol describing why an eval call was decided in SESSION.

Used only for logging.  The result is one of:
- `mode' if SESSION has an accept/reject mode set,
- `user' if SESSION has an `ask' mode set or `emcp-tools-eval-default-policy'
  is `ask' (the user is consulted via the confirmation buffer),
- `default' if `emcp-tools-eval-default-policy' is t or nil
  (auto-decided without asking)."
  (let ((mode (plist-get session :emcp-tools-eval-mode)))
    (cond
     ((memq mode '(accept reject)) 'mode)
     ((or (eq mode 'ask)
          (eq emcp-tools-eval-default-policy 'ask))
      'user)
     (t 'default))))

(defun emcp-tools-eval--eval-form (form send-result)
  "Evaluate FORM and call SEND-RESULT with the MCP tool result alist."
  (condition-case err
      (let* ((value (eval form t))
             ;; Send strings as is to avoid double quoting and
             ;; escaping newlines etc. This simplifies the output for
             ;; the agent and is usually not a problem, because the
             ;; agent wrote the code, so they know which type of value
             ;; to expect.
             (printed (if (stringp value) value (prin1-to-string value))))
        (funcall send-result
                 `((content . [((type . "text")
                                (text . ,(format "```emacs-lisp\n%s\n```" printed)))]))))
    (error
     (funcall send-result
              `((content . [((type . "text")
                             (text . ,(format "Error: %s"
                                              (error-message-string err))))])
                (isError . t))))))

(emcp-deftool emcp-tools-eval
    ((code "Multiple top-level forms are wrapped in an implicit `progn'."))
  "Evaluate Emacs Lisp.

To enhance the safety of letting an agent execute arbitrary code, every
tool call goes through a confirmation buffer.  The user can also enable
a session-wide accept or reject mode for trusted or untrusted sessions."
  :name "eval"
  :description "Evaluate Emacs Lisp code in the running Emacs instance and return the
value of the final form.

The return value is printed with prin1-to-string unless it is a string.
Strings are returned directly to simplify the output and avoid double
quoting and escape sequences.

Each evaluation is gated by a user-controlled approval system.  The call
may block until the user approves it.

Prefer a more specialized tool if available to minimize user interaction
and decision fatigue."
  :async t
  (condition-case err
      (let* ((form (emcp-tools-eval--parse code))
             (decision (emcp-tools-eval--authorize session))
             ;; Capture the source before the prompt opens: a mode-accept or mode-reject
             ;; pick by the user would otherwise relabel this call's source to `mode'.
             (source (emcp-tools-eval--decision-source session)))
        (cl-flet ((maybe-eval (accept)
                    (emcp--log server session
                      (info (emcp-tools-eval--format-log accept source form)))
                    (if accept
                        (emcp-tools-eval--eval-form form #'send-result)
                      (send-result
                       `((content . [((type . "text")
                                      (text . "User rejected evaluation."))])
                         (isError . t))))))
          (if (eq decision 'ask)
              (emcp-tools-eval--prompt server session form #'maybe-eval)
            (maybe-eval decision))))
    (emcp-tools-eval-parse-error
     (emcp--log server session
       (info (format "eval REJECT parse-error %s" (cadr err))))
     (send-result
      `((content . [((type . "text")
                     (text . ,(format "Parse error: %s" (cadr err))))])
        (isError . t))))))

(provide 'emcp-tools-eval)
;;; emcp-tools-eval.el ends here
