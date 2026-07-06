;;; emcp-session-manager.el --- Session manager UI for EMCP -*- lexical-binding: t -*-

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

;; A UI for managing client sessions across all running EMCP servers.  Allows quick access
;; to the server log, managing a session's code evaluation state and more.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'subr-x)

(require 'emcp-core)
(require 'emcp-tools-eval)
(require 'emcp-tools-send-keys)

;; Declared in emcp.el; avoid a circular `require'.
(defvar emcp--servers)
(declare-function emcp-server-url "emcp" (server))
(declare-function emcp-stop "emcp" (profile))

(defgroup emcp-session-manager ()
  "Session manager UI for EMCP."
  :group 'emcp)

(defface emcp-session-manager-server '((t :inherit magit-section-heading))
  "Face for server (profile) headings in the session manager."
  :group 'emcp-session-manager)

(defface emcp-session-manager-session '((t :inherit font-lock-function-name-face))
  "Face for session headings in the session manager."
  :group 'emcp-session-manager)

(defface emcp-session-manager-mode-active
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the currently selected mode in the session manager."
  :group 'emcp-session-manager)

(defface emcp-session-manager-mode-inactive '((t :inherit shadow))
  "Face for the non-selected modes in the session manager."
  :group 'emcp-session-manager)

(defface emcp-session-manager-key '((t :inherit font-lock-builtin-face))
  "Face for key hints in the session manager."
  :group 'emcp-session-manager)

(defcustom emcp-session-manager-buffer-name "*EMCP sessions*"
  "Name of the session manager buffer."
  :group 'emcp-session-manager
  :type 'string)

(defconst emcp-session-manager--modes '(accept reject ask)
  "Possible session-mode values shown in the session manager.

Cycling progresses through this list in order.")

(defun emcp-session-manager--propertize (string face)
  "Return STRING with FACE applied via both `face' and `font-lock-face'.

`magit-section-mode' enables a minimal font-lock setup that strips the
plain `face' property, so faces have to also be set under
`font-lock-face' to survive."
  (propertize string 'face face 'font-lock-face face))

(defvar-keymap emcp-session-manager-mode-map
  :doc "Keymap for `emcp-session-manager-mode'."
  :parent magit-section-mode-map
  "g" #'emcp-session-manager-refresh
  "e" #'emcp-session-manager-cycle-eval-mode
  "K" #'emcp-session-manager-cycle-send-keys-mode
  "l" #'emcp-session-manager-show-log
  "k" #'emcp-session-manager-kill)

(define-derived-mode emcp-session-manager-mode magit-section-mode "EMCP-Sessions"
  "Major mode for the EMCP session manager."
  :group 'emcp-session-manager)

;;; Effective-mode resolution

(defun emcp-session-manager--effective-mode (session key default-policy)
  "Return the effective mode for SESSION's KEY plist property.

If no session mode is set, fall back to DEFAULT-POLICY translated to
one of `accept', `reject' or `ask'."
  (let ((mode (plist-get session key)))
    (cond
     ((memq mode emcp-session-manager--modes) mode)
     ((eq default-policy t) 'accept)
     ((null default-policy) 'reject)
     (t 'ask))))

(defun emcp-session-manager--cycle (current)
  "Return the next mode after CURRENT in `emcp-session-manager--modes'."
  (or (cadr (memq current emcp-session-manager--modes))
      (car emcp-session-manager--modes)))

;;; Rendering helpers

(defun emcp-session-manager--key-hint (command)
  "Return a string describing the key bound to COMMAND in the session manager."
  (if-let* ((keys (where-is-internal command emcp-session-manager-mode-map t)))
      (key-description keys)
    (format "M-x %s" command)))

(defun emcp-session-manager--mode-line (label active command)
  "Render LABEL: mode1 mode2 mode3 with ACTIVE highlighted plus a hint for COMMAND."
  (concat
   (format "%-16s" (concat label ":"))
   (mapconcat
    (lambda (mode)
      (emcp-session-manager--propertize
       (symbol-name mode)
       (if (eq mode active)
           'emcp-session-manager-mode-active
         'emcp-session-manager-mode-inactive)))
    emcp-session-manager--modes
    " ")
   "   ("
   (emcp-session-manager--propertize (emcp-session-manager--key-hint command)
                                     'emcp-session-manager-key)
   " to cycle)"))

(defun emcp-session-manager--client-info (session)
  "Return a short \"name version\" string for SESSION's client, or nil."
  (when-let* ((info (plist-get session :client-info))
              ((hash-table-p info)))
    (let ((name (or (gethash "title" info) (gethash "name" info)))
          (version (gethash "version" info)))
      (cond
       ((and name version) (format "%s %s" name version))
       (name name)
       (t nil)))))

(defun emcp-session-manager--root-label (root)
  "Return a short label for ROOT, a plist as produced by `emcp--normalize-root'."
  (or (plist-get root :name)
      (when-let* ((path (plist-get root :path)))
        (abbreviate-file-name path))
      (plist-get root :uri)))

(defun emcp-session-manager--session-sort-key (session)
  "Return a sort key for SESSION."
  (list (or (plist-get (car (plist-get session :roots)) :path)
            (emcp--session-label session))
        (- (float-time (plist-get session :created)))))

;;; Section insertion

(defun emcp-session-manager--insert-session (profile session)
  "Insert a section for SESSION belonging to PROFILE's server."
  (let* ((id (plist-get session :id))
         (label (emcp--session-label session))
         (eval-mode (emcp-session-manager--effective-mode
                     session :emcp-tools-eval-mode
                     emcp-tools-eval-default-policy))
         (send-keys-mode (emcp-session-manager--effective-mode
                          session :emcp-tools-send-keys-mode
                          emcp-tools-send-keys-default-policy)))
    (magit-insert-section (emcp-session (cons profile id))
      (magit-insert-heading
        (emcp-session-manager--propertize label 'emcp-session-manager-session)
        (format "  [%s]" (substring id 0 (min 8 (length id)))))
      (insert (format "    State:          %s\n"
                      (or (plist-get session :state) "?")))
      (when-let* ((created (plist-get session :created)))
        (insert (format "    Created:        %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S" created))))
      (when-let* ((last-message (plist-get session :last-message-time)))
        (insert (format "    Last message:   %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S" last-message))))
      (when-let* ((client (emcp-session-manager--client-info session)))
        (insert (format "    Client:         %s\n" client)))
      (when-let* ((roots (plist-get session :roots)))
        (insert "    Roots:\n")
        (dolist (root roots)
          (insert (format "      %s\n" (emcp-session-manager--root-label root)))))
      (insert "    "
              (emcp-session-manager--mode-line
               "Eval" eval-mode #'emcp-session-manager-cycle-eval-mode)
              "\n")
      (insert "    "
              (emcp-session-manager--mode-line
               "Send-keys" send-keys-mode #'emcp-session-manager-cycle-send-keys-mode)
              "\n\n"))))

(defun emcp-session-manager--insert-log-line (server)
  "Insert a single-line section pointing at SERVER's log buffer."
  (let ((buf (emcp--server-log-buffer server)))
    (magit-insert-section (emcp-log buf)
      (magit-insert-heading
        "  Log: "
        (if (buffer-live-p buf) (buffer-name buf) "(killed)")
        "   ("
        (emcp-session-manager--propertize "l" 'emcp-session-manager-key)
        " to open)"))))

(defun emcp-session-manager--insert-server (entry)
  "Insert a section for ENTRY, a (PROFILE . SERVER) pair from `emcp--servers'."
  (let* ((profile (car entry))
         (server (cdr entry))
         (url (ignore-errors (emcp-server-url server)))
         (sessions (sort (hash-table-values (emcp--server-sessions server))
                         :key #'emcp-session-manager--session-sort-key)))
    (magit-insert-section (emcp-server profile)
      (magit-insert-heading
        (emcp-session-manager--propertize (format "[%s]" profile)
                                          'emcp-session-manager-server)
        (if url (concat "  " url) ""))
      (emcp-session-manager--insert-log-line server)
      (cond
       ((null sessions)
        (insert "  (no active sessions)\n"))
       (t
        (insert "\n")
        (dolist (session sessions)
          (emcp-session-manager--insert-session profile session))))
      (insert "\n"))))

(defun emcp-session-manager--render ()
  "Render the session manager buffer body."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (emcp-root)
      (magit-insert-heading "EMCP servers")
      (insert "\n")
      (cond
       ((null emcp--servers)
        (insert "No running servers.  Start one with `M-x emcp-start'.\n"))
       (t
        (dolist (entry emcp--servers)
          (emcp-session-manager--insert-server entry)))))))

;;; Section lookup

(defun emcp-session-manager--ancestor (type)
  "Return the nearest ancestor section of TYPE at point, or nil."
  (cl-loop for s = (magit-current-section) then (oref s parent)
           while s
           when (eq (oref s type) type)
           return s))

(defun emcp-session-manager--first-section-position (types)
  "Return the buffer position of the first section whose type is in TYPES.

TYPES is searched in order of preference: the position of the first
section of the first listed type is returned, falling back to subsequent
types.  Returns nil when no matching section exists."
  (cl-some
   (lambda (type)
     (let (found)
       (save-excursion
         (goto-char (point-min))
         (while (and (not found) (not (eobp)))
           (when-let* ((section (magit-section-at))
                       ((eq (oref section type) type)))
             (setq found (point)))
           (goto-char (or (next-single-property-change (point) 'magit-section)
                          (point-max)))))
       found))
   types))

(defun emcp-session-manager--session-at-point ()
  "Return (PROFILE SERVER SESSION) for the session at point or signal."
  (let ((section (emcp-session-manager--ancestor 'emcp-session)))
    (unless section
      (user-error "Point is not on a session"))
    (pcase-let* ((`(,profile . ,id) (oref section value))
                 (server (alist-get profile emcp--servers))
                 (session (and server (emcp--server-get-session server id))))
      (unless server
        (user-error "Server `%s' is no longer running" profile))
      (unless session
        (user-error "Session %s no longer exists" id))
      (list profile server session))))

(defun emcp-session-manager--server-at-point ()
  "Return (PROFILE . SERVER) for the server section enclosing point or signal."
  (let ((section (emcp-session-manager--ancestor 'emcp-server)))
    (unless section
      (user-error "Point is not on a server or session"))
    (let* ((profile (oref section value))
           (server (alist-get profile emcp--servers)))
      (unless server
        (user-error "Server `%s' is no longer running" profile))
      (cons profile server))))

;;; Commands

(defun emcp-session-manager-refresh ()
  "Refresh the session manager buffer in place."
  (interactive)
  (unless (derived-mode-p 'emcp-session-manager-mode)
    (user-error "Not in an EMCP session manager buffer"))
  (let ((line (line-number-at-pos))
        (col (current-column)))
    (emcp-session-manager--render)
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column col)))

(defun emcp-session-manager--cycle-mode (key default-policy label)
  "Cycle the session mode stored under KEY for the session at point.

DEFAULT-POLICY is the default policy custom for the corresponding tool and
LABEL is a string describing the mode for the user-facing message."
  (pcase-let* ((`(,_profile ,_server ,session)
                (emcp-session-manager--session-at-point))
               (current (emcp-session-manager--effective-mode
                         session key default-policy))
               (next (emcp-session-manager--cycle current)))
    (plist-put session key next)
    (emcp-session-manager-refresh)
    (message "%s mode for %s: %s -> %s"
             label (emcp--session-label session) current next)))

(defun emcp-session-manager-cycle-eval-mode ()
  "Cycle the eval-tool session mode for the session at point."
  (interactive)
  (emcp-session-manager--cycle-mode
   :emcp-tools-eval-mode emcp-tools-eval-default-policy "Eval"))

(defun emcp-session-manager-cycle-send-keys-mode ()
  "Cycle the send-keys-tool session mode for the session at point."
  (interactive)
  (emcp-session-manager--cycle-mode
   :emcp-tools-send-keys-mode emcp-tools-send-keys-default-policy "Send-keys"))

(defun emcp-session-manager-show-log ()
  "Open the log buffer of the server enclosing point."
  (interactive)
  (pcase-let* ((`(,profile . ,server) (emcp-session-manager--server-at-point))
               (buf (emcp--server-log-buffer server)))
    (unless (buffer-live-p buf)
      (user-error "Log buffer for `%s' no longer exists" profile))
    (pop-to-buffer buf)))

(defun emcp-session-manager-kill-server ()
  "Kill the server enclosing point after confirmation."
  (interactive)
  (pcase-let* ((`(,profile . ,_server) (emcp-session-manager--server-at-point))
               (name (emcp-session-manager--propertize (symbol-name profile)
                                                        'emcp-session-manager-server)))
    (when (yes-or-no-p (format "Kill server %s? " name))
      (emcp-stop profile)
      (emcp-session-manager-refresh)
      (message "Stopped %s" name))))

(defun emcp-session-manager-kill-session ()
  "Kill the session at point after confirmation."
  (interactive)
  (pcase-let* ((`(,_profile ,server ,session) (emcp-session-manager--session-at-point))
               (name (emcp-session-manager--propertize (emcp--session-label session)
                                                        'emcp-session-manager-session)))
    (when (yes-or-no-p (format "Kill session %s? " name))
      (when-let* ((channel (plist-get session :client-channel)))
        (funcall channel nil))
      (emcp--server-delete-session server session)
      (emcp-session-manager-refresh)
      (message "Killed session %s" name))))

(defun emcp-session-manager-kill ()
  "Kill the session or server at point after confirmation.

Kill the session when point is on a session, or the server when point is
on a server section but not one of its sessions."
  (interactive)
  (if (emcp-session-manager--ancestor 'emcp-session)
      (emcp-session-manager-kill-session)
    (emcp-session-manager-kill-server)))

;;;###autoload
(defun emcp-session-manager ()
  "Pop up the EMCP session manager.

Lists running EMCP servers and their active client sessions, lets the user
inspect and cycle each session's eval and send-keys mode in place, and gives
quick access to each server's log buffer."
  (interactive)
  (let ((buf (get-buffer-create emcp-session-manager-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'emcp-session-manager-mode)
        (emcp-session-manager-mode))
      (emcp-session-manager--render)
      (goto-char (or (emcp-session-manager--first-section-position
                      '(emcp-session emcp-server))
                     (point-min))))
    (pop-to-buffer buf)))

(provide 'emcp-session-manager)
;;; emcp-session-manager.el ends here
