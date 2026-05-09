;;; emcp-confirm.el --- Confirmation buffer for EMCP -*- lexical-binding: t -*-

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

;; A shared confirmation-buffer mechanic used by tools that need user sign-off before
;; acting (`eval', `send-keys', and any custom tools you build).  Tools provide a body to
;; display and a list of action groups, and `emcp-confirm-prompt' opens the buffer, builds
;; the keymap, and invokes the tool's callback with the chosen action's `:result' symbol.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup emcp-confirm ()
  "Confirmation buffer for EMCP tools."
  :group 'emcp)

(defcustom emcp-confirm-buffer-name "*EMCP confirm*"
  "Base name for the confirmation buffer.

Each prompt creates a fresh buffer via `generate-new-buffer', so concurrent
prompts get unique suffixed names like \"*EMCP confirm*<2>\".

Customize placement via `display-buffer-alist' with a regex that matches
this name and any numeric suffix, e.g. \"\\\\`\\\\*EMCP confirm\\\\*\"."
  :group 'emcp-confirm
  :type 'string)

(defvar-local emcp-confirm--pending nil
  "Plist of the buffer's pending request.

Keys: :session SESSION :context CONTEXT :callback CB :on-dismiss SYM.
Set to nil by `emcp-confirm--dispatch' before invoking the callback, so
the buffer-local `kill-buffer-hook' is a no-op on the decision path.")

(define-derived-mode emcp-confirm-mode special-mode "EMCP-confirm"
  "Major mode for EMCP confirmation buffers."
  (setq buffer-read-only t))

;; `q' must dismiss-and-invoke-callback rather than the special-mode default
;; `quit-window', which only buries the buffer and would leave the agent
;; hanging on an unanswered tool call.
(define-key emcp-confirm-mode-map (kbd "q") #'kill-current-buffer)

(defun emcp-confirm--render-group (group)
  "Render GROUP as a string of group title plus action cells.

GROUP is a plist with keys :title (optional string), :columns (optional
positive integer, defaults to the number of actions) and :actions (a
list of action cells of the form (KEY LABEL :result SYMBOL) or (KEY
LABEL :command FUNCTION)).  Actions are laid out column-major into
:columns columns, with each cell rendered as \"[KEY] LABEL\"."
  (let* ((title (plist-get group :title))
         (actions (plist-get group :actions))
         (n (length actions))
         (columns (or (plist-get group :columns) n))
         (rows (max 1 (ceiling n columns)))
         (cells (mapcar (lambda (action)
                          (format "[%c] %s" (nth 0 action) (nth 1 action)))
                        actions))
         (col-widths
          (cl-loop for c from 0 below columns
                   collect (cl-loop for r from 0 below rows
                                    for i = (+ (* c rows) r)
                                    when (< i n)
                                    maximize (length (nth i cells))))))
    (with-temp-buffer
      (insert "\n")
      (when title (insert title ":\n"))
      (dotimes (r rows)
        (let (parts)
          (cl-loop for c from 0 below columns
                   for i = (+ (* c rows) r)
                   when (< i n) do
                   (let* ((cell (nth i cells))
                          (cw (nth c col-widths))
                          (pad (- (+ cw 5) (length cell))))
                     (push (concat cell (make-string pad ?\s)) parts)))
          (when parts
            (insert "  "
                    (string-trim-right
                     (mapconcat #'identity (nreverse parts) ""))
                    "\n"))))
      (buffer-string))))

(defun emcp-confirm--format-body (body)
  "Return BODY indented by two spaces with a guaranteed trailing newline.

Empty or nil BODY returns the empty string unchanged so the layout
collapses cleanly when the caller has nothing to display."
  (if (or (null body) (string-empty-p body))
      ""
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (not (eobp))
        (insert "  ")
        (forward-line 1))
      (unless (eq (char-before) ?\n)
        (insert "\n"))
      (buffer-string))))

(defun emcp-confirm--render (session title body groups)
  "Render the confirmation buffer body.

SESSION is the MCP session plist; only its :id is used for the header.
TITLE is the verb that completes the header line.  BODY is a
caller-supplied string inserted between the header and the action menu;
it is indented by two spaces and given a trailing newline.  GROUPS is
the list of action groups passed through to `emcp-confirm--render-group'."
  (let ((session-id (or (plist-get session :id) "?")))
    (concat
     (format "The agent in session %s wants to %s:\n\n"
             (substring session-id 0 (min 8 (length session-id)))
             title)
     (emcp-confirm--format-body body)
     (mapconcat #'emcp-confirm--render-group groups ""))))

(defun emcp-confirm--build-keymap (groups)
  "Build a keymap binding each :result/:command action in GROUPS.

The returned keymap parents to `emcp-confirm-mode-map' so the `q'
binding (and other special-mode defaults) remain reachable."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map emcp-confirm-mode-map)
    (dolist (group groups)
      (dolist (action (plist-get group :actions))
        (let ((key (nth 0 action))
              (rest (cddr action)))
          (cond
           ((plist-get rest :result)
            (let ((result (plist-get rest :result)))
              (define-key map (vector key)
                          (lambda () (interactive) (emcp-confirm--dispatch result)))))
           ((plist-get rest :command)
            (define-key map (vector key) (plist-get rest :command)))))))
    map))

(defun emcp-confirm--dispatch (result)
  "Invoke the pending callback with RESULT and kill the buffer."
  (let* ((pending emcp-confirm--pending)
         (callback (plist-get pending :callback)))
    (unless pending
      (user-error "No pending confirmation request in this buffer"))
    ;; Clear pending first so the kill-buffer-hook is a no-op on this path.
    (setq emcp-confirm--pending nil)
    (let ((buf (current-buffer)))
      (funcall callback result)
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun emcp-confirm--on-kill ()
  "Buffer-local `kill-buffer-hook'.

If `emcp-confirm--pending' is still set when the buffer is killed, the
user dismissed without picking an action.  Invoke the callback with the
caller-supplied :on-dismiss symbol so the agent's tool call resolves
instead of hanging."
  (when-let* ((pending emcp-confirm--pending)
              (callback (plist-get pending :callback)))
    (setq emcp-confirm--pending nil)
    (funcall callback (plist-get pending :on-dismiss))))

(cl-defun emcp-confirm-prompt (&key session title body context groups
                                    on-dismiss callback)
  "Open the confirmation buffer to ask the user about a tool call.

SESSION is the MCP session plist.

TITLE is the verb that completes \"The agent in session ID wants to TITLE:\".

BODY is a string inserted between the header and the action menu.  It
is indented by two spaces and a trailing newline is added if missing,
so callers only need to handle font-locking when they want it.

CONTEXT is a plist stored buffer-locally so :command handlers can read
arbitrary per-prompt data (e.g. the original code for a copy command).

GROUPS is an ordered list of action groups.  Each group is a plist with:
  :title   Optional string heading.
  :columns Optional positive integer; defaults to the number of actions
           in the group, so they fit on one row.  Actions are laid out
           column-major in the given number of columns.
  :actions Non-empty list of (KEY LABEL :result SYMBOL) or
           (KEY LABEL :command FUNCTION) cells.  KEY is a character.  A
           :result action invokes CALLBACK with SYMBOL and dismisses the
           buffer.  A :command action runs FUNCTION (a no-arg
           interactive command) without dismissing the buffer.

CALLBACK is invoked exactly once with one of:
  - the chosen :result symbol, if the user picks a :result action;
  - the ON-DISMISS value, if the buffer is killed (via `q' or
    `kill-buffer') before any :result action runs.

Returns the buffer."
  (let ((buf (generate-new-buffer emcp-confirm-buffer-name)))
    (with-current-buffer buf
      ;; The major mode must be set before `emcp-confirm--pending' is
      ;; assigned because `define-derived-mode' runs `kill-all-local-variables'
      ;; in its body, which would clear the buffer-local pending plist.
      (emcp-confirm-mode)
      (let ((inhibit-read-only t))
        (insert (emcp-confirm--render session title body groups)))
      (use-local-map (emcp-confirm--build-keymap groups))
      (setq emcp-confirm--pending
            (list :session session :context context
                  :on-dismiss on-dismiss :callback callback))
      (add-hook 'kill-buffer-hook #'emcp-confirm--on-kill nil t))
    (pop-to-buffer buf '((display-buffer-in-side-window)
                         (side . bottom)
                         (window-height . 0.4)))
    buf))

(provide 'emcp-confirm)
;;; emcp-confirm.el ends here
