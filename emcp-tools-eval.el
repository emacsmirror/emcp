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

;; An MCP tool to allow agents to evaluate arbitrary Emacs Lisp.  On every call, the user
;; has to confirm or reject the execution.  Acceptance or rejection can be remembered per
;; session or permanently and the user can also enable always-accept or always-reject
;; modes for a session.

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

(defcustom emcp-tools-eval-default-policy 'query
  "Default action when no cached decision applies.

t      always accept without prompting,
nil    always reject without prompting,
query  open the confirmation buffer."
  :group 'emcp-tools-eval
  :type '(choice (const :tag "Always accept" t)
                 (const :tag "Always reject" nil)
                 (const :tag "Ask the user" query)))

(defcustom emcp-tools-eval-cache-file
  (locate-user-emacs-file "emcp/eval.eld")
  "File where persistent accept/reject decisions are stored.

The file contains a single sexp: an alist of (FORM . DECISION) pairs
where DECISION is t (accept) or nil (reject)."
  :group 'emcp-tools-eval
  :type 'file)

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

;;; Recorded decisions

(defvar emcp-tools-eval--decisions-cache nil
  "Cached value of the persistent decisions.")

(defun emcp-tools-eval--recorded-decisions ()
  "Return the persistent decisions alist, loading it lazily."
  (unless emcp-tools-eval--decisions-cache
    (setq emcp-tools-eval--decisions-cache
          (if (file-readable-p emcp-tools-eval-cache-file)
              (with-temp-buffer
                (insert-file-contents emcp-tools-eval-cache-file)
                (condition-case _ (read (current-buffer))
                  (error nil)))
            nil)))
  emcp-tools-eval--decisions-cache)

(defun emcp-tools-eval--record-decision (form decision)
  "Persistently record DECISION (t or nil) for FORM."
  (let ((alist (emcp-tools-eval--recorded-decisions)))
    (setf (alist-get form alist nil nil #'equal) decision)
    (setq emcp-tools-eval--decisions-cache alist)
    (emcp-tools-eval--write-decisions alist)))

(defun emcp-tools-eval--write-decisions (alist)
  "Write ALIST to `emcp-tools-eval-cache-file'."
  (let ((dir (file-name-directory emcp-tools-eval-cache-file)))
    (unless (file-directory-p dir) (make-directory dir t)))
  (with-temp-file emcp-tools-eval-cache-file
    (insert ";;; -*- lexical-binding: t -*-\n")
    (insert (pp-to-string alist))))

;;; Authorization

(defun emcp-tools-eval--authorize (form session persistent-alist)
  "Authorize FORM in SESSION against the persistent and session caches.

PERSISTENT-ALIST is the persistent decisions alist.  Return t (accept),
nil (reject), or `prompt' (ask user)."
  (let ((mode (plist-get session :emcp-tools-eval-mode))
        ;; `assoc' (not `alist-get') so we can tell a miss from an
        ;; explicit nil (reject) decision.
        (sc (assoc form (plist-get session :emcp-tools-eval-cache)))
        (pc (assoc form persistent-alist)))
    (cond
     ((eq mode 'accept) t)
     ((eq mode 'reject) nil)
     (sc (cdr sc))
     (pc (cdr pc))
     ((eq emcp-tools-eval-default-policy 'query) 'prompt)
     (t emcp-tools-eval-default-policy))))

;;; Confirmation buffer

(defun emcp-tools-eval--copy-code-to-kill-ring ()
  "Copy the code under review to the `kill-ring' without closing the buffer."
  (interactive)
  (kill-new (plist-get (plist-get emcp-confirm--pending :context) :code))
  (message "EMCP: code copied to kill-ring"))

(defun emcp-tools-eval--apply-action (action session form)
  "Convert ACTION into a decision about FORM.

Depending on ACTION, maybe store the decision in the SESSION or
persistent cache."
  (cl-flet ((cache-session (decision)
              (let ((cache (plist-get session :emcp-tools-eval-cache)))
                (setf (alist-get form cache nil nil #'equal) decision)
                (plist-put session :emcp-tools-eval-cache cache))))
    (pcase action
      ('yes-once    t)
      ('no-once     nil)
      ('yes-session (cache-session t) t)
      ('no-session  (cache-session nil) nil)
      ('yes-always  (emcp-tools-eval--record-decision form t) t)
      ('no-always   (emcp-tools-eval--record-decision form nil) nil)
      ('mode-accept (plist-put session :emcp-tools-eval-mode 'accept) t)
      ('mode-reject (plist-put session :emcp-tools-eval-mode 'reject) nil))))

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
         :columns 3
         :actions ((?y "Yes once"           :result yes-once)
                   (?n "No once"            :result no-once)
                   (?Y "Yes this session"   :result yes-session)
                   (?N "No this session"    :result no-session)
                   (?! "Yes always (saved)" :result yes-always)
                   (?~ "No always (saved)"  :result no-always)))
       ( :title "Session mode (applies to all subsequent eval calls in this session)"
         :actions ((?a "Always accept" :result mode-accept)
                   (?r "Always reject" :result mode-reject)))
       ( :actions ((?w "Copy code to kill-ring"
                       :command emcp-tools-eval--copy-code-to-kill-ring))))
     :callback (lambda (action)
                 (funcall callback
                          (emcp-tools-eval--apply-action action session form))))))

;;; Decision logging

(defun emcp-tools-eval--format-log (decision reason form)
  "Format a single decision-log line.

DECISION is t (accept) or nil (reject).  REASON is a symbol describing
why this decision was made (e.g. `user', `cache:sess', `mode',
`default').  FORM is the form that was decided on."
  (let ((pp (string-trim (pp-to-string form))))
    (format "eval %s %s %s"
            (if decision "ACCEPT" "REJECT")
            (symbol-name reason)
            (if (> (length pp) 80) (concat (substring pp 0 77) "...") pp))))

;;; The tool

(defun emcp-tools-eval--decision-source (session form)
  "Return a symbol describing why FORM was decided in SESSION.

Used only for logging.  The result is one of `mode', `cache:sess',
`cache:persist', or `default'."
  (let ((mode (plist-get session :emcp-tools-eval-mode)))
    (cond
     ((memq mode '(accept reject)) 'mode)
     ((assoc form (plist-get session :emcp-tools-eval-cache)) 'cache:sess)
     ((assoc form (emcp-tools-eval--recorded-decisions)) 'cache:persist)
     (t 'default))))

(defun emcp-tools-eval--eval-form (form send-result)
  "Evaluate FORM and call SEND-RESULT with the MCP tool result alist."
  (condition-case err
      (let ((value (eval form t)))
        (funcall send-result
                 `((content . [((type . "text")
                                (text . ,(prin1-to-string value)))]))))
    (error
     (funcall send-result
              `((content . [((type . "text")
                             (text . ,(format "Error: %s"
                                              (error-message-string err))))])
                (isError . t))))))

(emcp-deftool emcp-tools-eval
    ((code "Multiple top-level forms are wrapped in an implicit `progn'."))
  "Evaluate Emacs Lisp.

To enhance the safety of letting an agent execute arbitrary code,
every tool call goes through a layered confirmation system:
1. If the session is in =accept= or =reject= mode, do that
2. If that particular code has been accepted or rejected for this
   session or permanently, do that
3. Otherwise, show the formatted code in a buffer and ask the user"
  :name "eval"
  :description "Evaluate Emacs Lisp code in the running Emacs instance and return the
value of the final form.

Each evaluation is gated by a user-controlled approval system.  The call
may block until the user approves it.

Prefer a more specialized tool if available to minimize user interaction
and decision fatigue."
  :async t
  (condition-case err
      (let* ((form (emcp-tools-eval--parse code))
             (decision (emcp-tools-eval--authorize
                        form session (emcp-tools-eval--recorded-decisions))))
        (cl-flet ((maybe-eval (accept reason)
                    (emcp--log server session
                      (info (emcp-tools-eval--format-log accept reason form)))
                    (if accept
                        (emcp-tools-eval--eval-form form #'send-result)
                      (send-result
                       `((content . [((type . "text")
                                      (text . "User rejected evaluation."))])
                         (isError . t))))))
          (pcase decision
            ('prompt
             (emcp-tools-eval--prompt
              server session form
              (lambda (d) (maybe-eval d 'user))))
            (_
             (maybe-eval decision (emcp-tools-eval--decision-source session form))))))
    (emcp-tools-eval-parse-error
     (emcp--log server session
       (info (format "eval REJECT parse-error %s" (cadr err))))
     (send-result
      `((content . [((type . "text")
                     (text . ,(format "Parse error: %s" (cadr err))))])
        (isError . t))))))

(provide 'emcp-tools-eval)
;;; emcp-tools-eval.el ends here
