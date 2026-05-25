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
(require 'seq)
(require 'subr-x)

(require 'emcp-core)

(defgroup emcp-confirm ()
  "Confirmation buffer for EMCP tools."
  :group 'emcp)

(defface emcp-confirm-title '((t :inherit font-lock-function-name-face))
  "Face used for the verb in the confirmation buffer's header line."
  :group 'emcp-confirm)

(defface emcp-confirm-agent '((t :inherit font-lock-keyword-face))
  "Face used for the agent label in the confirmation buffer's header line."
  :group 'emcp-confirm)

(defface emcp-confirm-key '((t :inherit font-lock-builtin-face))
  "Face used for action keys in the confirmation buffer."
  :group 'emcp-confirm)

(defcustom emcp-confirm-buffer-name "*EMCP confirm*"
  "Base name for the confirmation buffer.

Each prompt creates a fresh buffer via `generate-new-buffer', so concurrent
prompts get unique suffixed names like \"*EMCP confirm*<2>\".

Customize placement via `display-buffer-alist' with a regex that matches
this name and any numeric suffix, e.g. \"\\\\`\\\\*EMCP confirm\\\\*\"."
  :group 'emcp-confirm
  :type 'string)

(defcustom emcp-confirm-input-delay 0.2
  "Seconds to ignore input after a confirmation buffer opens.

Prevents accidentally choosing an action when the buffer pops up while
you are typing in another buffer (e.g. hitting y or n mid-word).
Keystrokes that arrive during this grace period are dropped with a
message in the echo area; the buffer accepts input normally once the
delay has elapsed.  Set to nil or 0 to disable."
  :group 'emcp-confirm
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds")))

(defvar-local emcp-confirm--pending nil
  "Plist of the buffer's pending request.

Keys: :server SERVER :session SESSION :context CONTEXT :callback CB
:on-dismiss SYM.  Set to nil by `emcp-confirm--dispatch' before invoking
the callback, so the buffer-local `kill-buffer-hook' is a no-op on the
decision path.")

(defvar-local emcp-confirm--ready-time nil
  "Time after which the confirm buffer accepts input.")

(defun emcp-confirm--block-input ()
  "Buffer-local `pre-command-hook' that drops input during the grace period."
  (if-let* ((ready emcp-confirm--ready-time)
            ((time-less-p (current-time) ready)))
      (progn
        (setq this-command #'ignore)
        (message "Ignoring input for %dms (customize with emcp-confirm-input-delay)"
                 (round emcp-confirm-input-delay 0.001)))
    (setq emcp-confirm--ready-time nil)
    (remove-hook 'pre-command-hook #'emcp-confirm--block-input t)))

(define-derived-mode emcp-confirm-mode special-mode "EMCP-confirm"
  "Major mode for EMCP confirmation buffers."
  (setq buffer-read-only t))

;; `q' must dismiss-and-invoke-callback rather than the special-mode default
;; `quit-window', which only buries the buffer and would leave the agent
;; hanging on an unanswered tool call.
(define-key emcp-confirm-mode-map (kbd "q") #'kill-current-buffer)

(defun emcp-confirm--cells (group)
  "Return GROUP's actions formatted as \"[KEY] LABEL\" cells."
  (mapcar (lambda (action)
            (format "[%s] %s"
                    (propertize (string (nth 0 action))
                                'face 'emcp-confirm-key)
                    (nth 1 action)))
          (plist-get group :actions)))

(defun emcp-confirm--render-groups (groups)
  "Render GROUPS as titled rows of action cells, column-aligned across groups.

GROUPS is an ordered list of action groups (see `emcp-confirm-prompt').
Each group renders as an optional title line followed by a single row
of \"[KEY] LABEL\" cells.  Cells at the same position in different
groups are padded to a common width so they line up vertically."
  (let* ((groups-cells (mapcar #'emcp-confirm--cells groups))
         (n-cols (apply #'max 0 (mapcar #'length groups-cells)))
         (col-widths
          (cl-loop for c from 0 below n-cols
                   collect (or (cl-loop for cells in groups-cells
                                        ;; The last cell of each row does not contribute
                                        ;; to its column's width, so an unusually wide
                                        ;; trailing cell (or a singleton-row cell) cannot
                                        ;; inflate the layout.
                                        when (< c (1- (length cells)))
                                        maximize (length (nth c cells)))
                               0))))
    (with-temp-buffer
      (cl-loop
       for group in groups
       for cells in groups-cells do
       (insert "\n")
       (when-let* ((title (plist-get group :title)))
         (insert title ":\n"))
       (when cells
         (let ((parts (cl-mapcar
                       (lambda (cell cw)
                         (concat cell
                                 (make-string (max 0 (- cw (length cell))) ?\s)))
                       cells col-widths)))
           (insert "  "
                   (string-trim-right (mapconcat #'identity parts "   "))
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

SESSION is the MCP session plist.  TITLE is the verb that completes the
header line.  BODY is a caller-supplied string describing what the agent
wants to TITLE.  GROUPS is a list of action groups that the user can
take."
  (concat
   (format "%s wants to %s:\n\n"
           (propertize (emcp--session-label session) 'face 'emcp-confirm-agent)
           (propertize title 'face 'emcp-confirm-title))
   (emcp-confirm--format-body body)
   (emcp-confirm--render-groups groups)))

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

;;; Desktop notifications

(declare-function notifications-notify "notifications")

(defun emcp-confirm--focus-buffer-frame (buffer)
  "Raise and focus the frame displaying BUFFER, if any."
  (when (buffer-live-p buffer)
    (when-let* ((win (get-buffer-window buffer t))
                (frame (window-frame win)))
      (raise-frame frame)
      (select-frame-set-input-focus frame))))

(defun emcp-confirm-notify-notifications (title body buffer)
  "Display a notification via the Freedesktop D-Bus notification service.

Uses Emacs's built-in `notifications-notify'.  TITLE and BODY are the
notification title and message text.  Clicking the notification (or its
\"Show\" action) raises and focuses the frame displaying BUFFER, if it
is still alive."
  (notifications-notify
   :title title :body body :app-name "EMCP"
   :actions '("default" "Show")
   :on-action (lambda (_id _key)
                (emcp-confirm--focus-buffer-frame buffer))))

(defcustom emcp-confirm-notify-darwin-bundle-id "org.gnu.Emacs"
  "Bundle identifier used to activate Emacs from a notification click.

Passed via the `-activate' flag of `terminal-notifier' so clicking the
notification brings Emacs to the foreground.  Override this if your
Emacs build uses a different bundle ID (check with =mdls -name
kMDItemCFBundleIdentifier /Applications/Emacs.app=)."
  :group 'emcp-confirm
  :type 'string)

(defun emcp-confirm-notify-terminal-notifier (title body _buffer)
  "Display a notification via the macOS \"terminal-notifier\" CLI.

TITLE and BODY are the notification title and message text.  Clicking
the notification activates Emacs (bundle ID
`emcp-confirm-notify-darwin-bundle-id').  BUFFER is unused because
terminal-notifier only supports app-level activation, not per-frame
focus.

Install the tool with =brew install terminal-notifier=."
  (call-process "terminal-notifier" nil 0 nil
                "-title" title
                "-message" body
                "-activate" emcp-confirm-notify-darwin-bundle-id))

(declare-function ns-do-applescript "nsfns.m")

(defun emcp-confirm-notify-applescript (title body _buffer)
  "Display a notification on macOS via AppleScript.

Uses the built-in `ns-do-applescript' available in Cocoa Emacs.  TITLE
and BODY are the notification title and message text.  BUFFER is ignored
because macOS \"display notification\" does not support click actions;
switch to `emcp-confirm-notify-terminal-notifier' for click-to-focus."
  (cl-flet ((quote-str (str)
              (concat "\""
                      (replace-regexp-in-string "[\\\"]" "\\\\\\&" str)
                      "\"")))
    (ns-do-applescript
     (format "display notification %s with title %s"
             (quote-str body)
             (quote-str title)))))

(defun emcp-confirm-notify-default (title body buffer)
  "Forward TITLE, BODY and BUFFER to an available built-in backend.

On macOS, prefer \"terminal-notifier\" when installed because clicks
activate Emacs; otherwise fall back to the AppleScript bridge.  On
other systems with D-Bus available, use `notifications-notify'.  If no
backend is available, do nothing."
  (cond
   ((and (featurep 'dbusbind) (require 'notifications nil t))
    (emcp-confirm-notify-notifications title body buffer))
   ((and (eq system-type 'darwin) (executable-find "terminal-notifier"))
    (emcp-confirm-notify-terminal-notifier title body buffer))
   ((and (eq system-type 'darwin) (fboundp 'ns-do-applescript))
    (emcp-confirm-notify-applescript title body buffer))))

(defcustom emcp-confirm-notify-function #'emcp-confirm-notify-default
  "Function called to notify the user about a pending confirmation.

Invoked with three arguments TITLE, BODY and BUFFER when a confirmation
buffer opens while no Emacs frame has focus.  BUFFER is the confirm
buffer; backends may use it to wire up click-to-focus actions.

Set to nil to disable desktop notifications.

Built-in choices:
  `emcp-confirm-notify-notifications'      D-Bus via `notifications-notify'.
  `emcp-confirm-notify-terminal-notifier'  macOS via \"terminal-notifier\".
  `emcp-confirm-notify-applescript'        macOS via `ns-do-applescript'.

`emcp-confirm-notify-default' picks the first available built-in from
this list."
  :group 'emcp-confirm
  :type '(choice (const :tag "Disabled" nil)
                 (function :tag "Notification function")))

(defun emcp-confirm--emacs-focused-p ()
  "Return non-nil if any Emacs frame currently has focus."
  (seq-some #'frame-focus-state (frame-list)))

(defun emcp-confirm--maybe-notify (server session title buffer)
  "Notify the user if Emacs is unfocused and a notifier is configured.

SERVER, SESSION and TITLE are the values passed to `emcp-confirm-prompt'.
BUFFER is the confirm buffer; backends may use it to wire up click
actions that focus the frame displaying it."
  (when (and emcp-confirm-notify-function
             (not (emcp-confirm--emacs-focused-p)))
    (condition-case err
        (funcall emcp-confirm-notify-function
                 "EMCP confirmation"
                 (format "%s wants to %s"
                         (emcp--session-label session)
                         title)
                 buffer)
      (error
       (emcp--log server session
         (warning (format "Notification failed: %s"
                          (error-message-string err))))))))

(cl-defun emcp-confirm-prompt (server session &key title body context groups
                                      on-dismiss callback)
  "Open the confirmation buffer to ask the user about a tool call.

SERVER is the MCP server struct and SESSION is the MCP session plist.

TITLE is the verb that completes \"The agent in session ID wants to TITLE:\".

BODY is a string inserted between the header and the action menu.  It
is indented by two spaces and a trailing newline is added if missing,
so callers only need to handle font-locking when they want it.

CONTEXT is a plist stored buffer-locally so :command handlers can read
arbitrary per-prompt data (e.g. the original code for a copy command).

GROUPS is an ordered list of action groups.  Each group is a plist with:
  :title   Optional string heading.
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
      (goto-char (point-min))
      (use-local-map (emcp-confirm--build-keymap groups))
      (setq emcp-confirm--pending
            (list :server server :session session :context context
                  :on-dismiss on-dismiss :callback callback))
      (add-hook 'kill-buffer-hook #'emcp-confirm--on-kill nil t)
      ;; Prevent input for a configurable time to avoid accidental decisions
      (when (and (numberp emcp-confirm-input-delay)
                 (> emcp-confirm-input-delay 0))
        (setq emcp-confirm--ready-time
              (time-add (current-time) emcp-confirm-input-delay))
        (add-hook 'pre-command-hook #'emcp-confirm--block-input nil t)))
    (pop-to-buffer buf '((display-buffer-in-side-window)
                         (side . bottom)
                         (window-height . 0.4)))
    (emcp-confirm--maybe-notify server session title buf)
    buf))

(provide 'emcp-confirm)
;;; emcp-confirm.el ends here
