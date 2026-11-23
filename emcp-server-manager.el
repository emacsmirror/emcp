;;; emcp-server-manager.el --- Server manager UI for EMCP -*- lexical-binding: t -*-

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

;; A UI for managing all running EMCP servers.  Allows quick access to each server's log,
;; cycling its eval and send-keys modes and stopping it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'magit-section)
(require 'subr-x)

(require 'emcp-core)
(require 'emcp-tools-eval)
(require 'emcp-tools-send-keys)

;; Declared in emcp.el; avoid a circular `require'.
(defvar emcp--servers)
(declare-function emcp-server-url "emcp" (server))
(declare-function emcp-stop "emcp" (profile))

(defgroup emcp-server-manager ()
  "Server manager UI for EMCP."
  :group 'emcp)

(defface emcp-server-manager-server '((t :inherit magit-section-heading))
  "Face for server (profile) headings in the server manager."
  :group 'emcp-server-manager)

(defface emcp-server-manager-mode-active
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the currently selected mode in the server manager."
  :group 'emcp-server-manager)

(defface emcp-server-manager-mode-inactive '((t :inherit shadow))
  "Face for the non-selected modes in the server manager."
  :group 'emcp-server-manager)

(defface emcp-server-manager-key '((t :inherit font-lock-builtin-face))
  "Face for key hints in the server manager."
  :group 'emcp-server-manager)

(defcustom emcp-server-manager-buffer-name "*EMCP servers*"
  "Name of the server manager buffer."
  :group 'emcp-server-manager
  :type 'string)

(defconst emcp-server-manager--modes '(accept reject ask)
  "Possible values of a server mode shown in the server manager.

Cycling progresses through this list in order.")

(defun emcp-server-manager--propertize (string face)
  "Return STRING with FACE applied via both `face' and `font-lock-face'.

`magit-section-mode' enables a minimal font-lock setup that strips the
plain `face' property, so faces have to also be set under
`font-lock-face' to survive."
  (propertize string 'face face 'font-lock-face face))

(defvar-keymap emcp-server-manager-mode-map
  :doc "Keymap for `emcp-server-manager-mode'."
  :parent magit-section-mode-map
  "g" #'emcp-server-manager-refresh
  "e" #'emcp-server-manager-cycle-eval-mode
  "K" #'emcp-server-manager-cycle-send-keys-mode
  "l" #'emcp-server-manager-show-log
  "k" #'emcp-server-manager-kill-server)

(define-derived-mode emcp-server-manager-mode magit-section-mode "EMCP-Servers"
  "Major mode for the EMCP server manager.

\\{emcp-server-manager-mode-map}"
  :group 'emcp-server-manager)

;;; Effective-mode resolution

(defun emcp-server-manager--effective-mode (server key default-policy)
  "Return the effective mode stored under KEY in SERVER.

If no server mode is set, fall back to DEFAULT-POLICY translated to
one of `accept', `reject' or `ask'."
  (let ((mode (emcp--server-mode server key)))
    (cond
     ((memq mode emcp-server-manager--modes) mode)
     ((eq default-policy t) 'accept)
     ((null default-policy) 'reject)
     (t 'ask))))

(defun emcp-server-manager--cycle (current)
  "Return the next mode after CURRENT in `emcp-server-manager--modes'."
  (or (cadr (memq current emcp-server-manager--modes))
      (car emcp-server-manager--modes)))

;;; Rendering

(defun emcp-server-manager--key-hint (command)
  "Return a string describing the key bound to COMMAND in the server manager."
  (if-let* ((keys (where-is-internal command emcp-server-manager-mode-map t)))
      (key-description keys)
    (format "M-x %s" command)))

(defun emcp-server-manager--mode-line (label active command)
  "Render LABEL: mode1 mode2 mode3 with ACTIVE highlighted plus a hint for COMMAND."
  (concat
   (format "%-16s" (concat label ":"))
   (mapconcat
    (lambda (mode)
      (emcp-server-manager--propertize
       (symbol-name mode)
       (if (eq mode active)
           'emcp-server-manager-mode-active
         'emcp-server-manager-mode-inactive)))
    emcp-server-manager--modes
    " ")
   "   ("
   (emcp-server-manager--propertize (emcp-server-manager--key-hint command)
                                    'emcp-server-manager-key)
   " to cycle)"))

(defun emcp-server-manager--insert-mode-lines (server)
  "Insert the eval and send-keys mode lines of SERVER."
  (insert "  "
          (emcp-server-manager--mode-line
           "Eval"
           (emcp-server-manager--effective-mode
            server :emcp-tools-eval-mode emcp-tools-eval-default-policy)
           #'emcp-server-manager-cycle-eval-mode)
          "\n  "
          (emcp-server-manager--mode-line
           "Send-keys"
           (emcp-server-manager--effective-mode
            server :emcp-tools-send-keys-mode emcp-tools-send-keys-default-policy)
           #'emcp-server-manager-cycle-send-keys-mode)
          "\n"))

(defun emcp-server-manager--insert-log-line (server)
  "Insert a single-line section pointing at SERVER's log buffer."
  (let ((buf (emcp--server-log-buffer server)))
    (magit-insert-section (emcp-log buf)
      (magit-insert-heading
        "  Log: "
        (if (buffer-live-p buf) (buffer-name buf) "(killed)")
        "   ("
        (emcp-server-manager--propertize
         (emcp-server-manager--key-hint #'emcp-server-manager-show-log)
         'emcp-server-manager-key)
        " to open)"))))

(defun emcp-server-manager--insert-server (entry)
  "Insert a section for ENTRY, a (PROFILE . SERVER) pair from `emcp--servers'."
  (pcase-let ((`(,profile . ,server) entry))
    (magit-insert-section (emcp-server profile)
      (magit-insert-heading
        (emcp-server-manager--propertize (format "[%s]" profile)
                                         'emcp-server-manager-server)
        (if-let* ((url (ignore-errors (emcp-server-url server))))
            (concat "  " url)
          ""))
      (emcp-server-manager--insert-log-line server)
      (emcp-server-manager--insert-mode-lines server)
      (insert "\n"))))

(defun emcp-server-manager--render ()
  "Render the server manager buffer body."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (emcp-root)
      (magit-insert-heading "EMCP servers")
      (insert "\n")
      (if emcp--servers
          ;; Sort by name, so that servers keep their position when others start or stop
          (mapc #'emcp-server-manager--insert-server
                (seq-sort-by #'car #'string< emcp--servers))
        (insert "No running servers.  Start one with `M-x emcp-start'.\n")))))

;;; Section lookup

(defun emcp-server-manager--server-section-at-point ()
  "Return the server section enclosing point or nil."
  (cl-loop for s = (magit-current-section) then (oref s parent)
           while s
           when (eq (oref s type) 'emcp-server)
           return s))

(defun emcp-server-manager--server-at-point ()
  "Return (PROFILE . SERVER) for the server section enclosing point or signal."
  (let ((section (emcp-server-manager--server-section-at-point)))
    (unless section
      (user-error "Point is not on a server"))
    (let* ((profile (oref section value))
           (server (alist-get profile emcp--servers)))
      (unless server
        (user-error "Server `%s' is no longer running" profile))
      (cons profile server))))

;;; Commands

(defun emcp-server-manager-refresh ()
  "Refresh the server manager buffer in place.

Keep point in the same server section, so that commands after a refresh
act on the same server even if servers above it started or stopped."
  (interactive)
  (unless (derived-mode-p 'emcp-server-manager-mode)
    (user-error "Not in an EMCP server manager buffer"))
  (let* ((section (emcp-server-manager--server-section-at-point))
         (profile (and section (oref section value)))
         (offset (and section (count-lines (oref section start) (line-beginning-position))))
         (line (line-number-at-pos))
         (col (current-column)))
    (emcp-server-manager--render)
    (if-let* ((profile)
              (new (seq-find (lambda (s) (eq (oref s value) profile))
                             (oref magit-root-section children))))
        (progn
          (goto-char (oref new start))
          (forward-line offset))
      (goto-char (point-min))
      (forward-line (1- line)))
    (move-to-column col)))

(defun emcp-server-manager--cycle-mode (key default-policy label)
  "Cycle the server mode stored under KEY for the server enclosing point.

DEFAULT-POLICY is the default policy custom for the corresponding tool and
LABEL is a string describing the mode for the user-facing message."
  (pcase-let* ((`(,profile . ,server) (emcp-server-manager--server-at-point))
               (current (emcp-server-manager--effective-mode
                         server key default-policy))
               (next (emcp-server-manager--cycle current)))
    (setf (emcp--server-mode server key) next)
    (emcp-server-manager-refresh)
    (message "%s mode for %s: %s -> %s" label profile current next)))

(defun emcp-server-manager-cycle-eval-mode ()
  "Cycle the eval-tool mode of the server enclosing point."
  (interactive)
  (emcp-server-manager--cycle-mode
   :emcp-tools-eval-mode emcp-tools-eval-default-policy "Eval"))

(defun emcp-server-manager-cycle-send-keys-mode ()
  "Cycle the send-keys-tool mode of the server enclosing point."
  (interactive)
  (emcp-server-manager--cycle-mode
   :emcp-tools-send-keys-mode emcp-tools-send-keys-default-policy "Send-keys"))

(defun emcp-server-manager-show-log ()
  "Open the log buffer of the server enclosing point."
  (interactive)
  (pcase-let* ((`(,profile . ,server) (emcp-server-manager--server-at-point))
               (buf (emcp--server-log-buffer server)))
    (unless (buffer-live-p buf)
      (user-error "Log buffer for `%s' no longer exists" profile))
    (pop-to-buffer buf)))

(defun emcp-server-manager-kill-server ()
  "Stop the server enclosing point after confirmation."
  (interactive)
  (pcase-let* ((`(,profile . ,_server) (emcp-server-manager--server-at-point))
               (name (emcp-server-manager--propertize (symbol-name profile)
                                                      'emcp-server-manager-server)))
    (when (yes-or-no-p (format "Kill server %s? " name))
      (emcp-stop profile)
      (emcp-server-manager-refresh)
      (message "Stopped %s" name))))

;;;###autoload
(defun emcp-server-manager ()
  "Pop up the EMCP server manager.

Lists running EMCP servers, lets the user inspect and cycle each
server's eval and send-keys mode in place, and gives quick access to
each server's log buffer."
  (interactive)
  (let ((buf (get-buffer-create emcp-server-manager-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'emcp-server-manager-mode)
        (emcp-server-manager-mode))
      (emcp-server-manager--render)
      (goto-char (if-let* ((first-server (car (oref magit-root-section children))))
                     (oref first-server start)
                   (point-min))))
    (pop-to-buffer buf)))

(provide 'emcp-server-manager)
;;; emcp-server-manager.el ends here
