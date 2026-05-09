;;; emcp.el --- Lets your agent talk to Emacs -*- lexical-binding: t -*-

;; Author: Marten Lienen <ml@martenlienen.com>
;; URL: https://codeberg.org/martenlienen/emcp
;; Keywords: maint
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (http-server "0.1.0"))

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

;; EMCP lets you connect your LLM agent directly to Emacs through an MCP server.  The
;; agent can look up documentation and definitions, take screenshots, read buffers,
;; execute code and more.  Exactly which prompts, resources and tools are available to the
;; agent depends on the active profile.

;;; Code:

(require 'cl-lib)
(require 'http-server)
(require 'rx)
(require 'seq)

(require 'emcp-core)
(require 'emcp-http)

;;; Public interface

(require 'emcp-prompts)
(require 'emcp-resources)
(require 'emcp-tools)
(require 'emcp-tools-eval)

(defcustom emcp-profiles
  '((inspect . ( :description "Ask Emacs about itself (docs, definitions, info pages).

Lets the agent search through symbols and look up their doc strings and
definitions. The agent can also search and read info pages.

The =/screenshot= prompt lets the user share a screenshot of all current
Emacs frames with the agent, without the agent being able to request
screenshots at will."
                 :prompts (emcp-prompt-screenshot)
                 :resources (emcp-resource-info-node)
                 :tools (emcp-tools-apropos
                         emcp-tools-describe
                         emcp-tools-find-definition
                         emcp-tools-info-search)))
    (develop . ( :description "Capabilities for developing Emacs lisp.

This gives the agent additional capabilities to work with Emacs and
inspect its current state like:
1. Taking screenshots"
                 :include (inspect)
                 :tools (emcp-tools-screenshot)))
    (full-control . ( :description "Full control over Emacs.

This allows arbitrary code evaluation protected by a user confirmation
interface."
                      :include (inspect develop)
                      :tools (emcp-tools-eval))))
  "Profiles of prompts, resources and tools.

:include includes the given profiles when the current one is started,
following :include recursively."
  :group 'emcp
  :type '(alist :key-type symbol
                :value-type (plist :key-type symbol
                                   :options
                                   ((:description string)
                                    (:include (repeat symbol))
                                    (:prompts (repeat symbol))
                                    (:resources (repeat symbol))
                                    (:tools (repeat symbol))))))

(defcustom emcp-default-profile nil
  "Default profile to start."
  :group 'emcp
  :type 'symbol)

(defvar emcp--servers ()
  "Running servers as a (PROFILE . SERVER) alist.")

(defun emcp--resolve-profile (profile)
  "Return a copy of PROFILE with :include resolved.

PROFILE is a plist as stored in `emcp-profiles'.  Follow :include links
recursively, merging :prompts, :resources and :tools from all included
profiles.  When the same MCP name appears more than once, the last
definition wins, so a profile can override items from included profiles."
  (let (visited prompts resources tools)
    (cl-labels ((resolve (p)
                  (setq prompts (append prompts (plist-get p :prompts)))
                  (setq resources (append resources (plist-get p :resources)))
                  (setq tools (append tools (plist-get p :tools)))
                  (dolist (name (plist-get p :include))
                    (unless (memq name visited)
                      (push name visited)
                      (if-let* ((included (alist-get name emcp-profiles)))
                          (resolve included)
                        (user-error "Unknown profile: %s" name))))))
      (resolve profile))
    (let ((result (cl-copy-list profile)))
      (cl-remf result :include)
      (plist-put result :prompts (emcp--dedup-by-name prompts '(emcp-prompt)))
      (plist-put result :resources (emcp--dedup-by-name resources '(emcp-resource emcp-resource-template)))
      (plist-put result :tools (emcp--dedup-by-name tools '(emcp-tool))))))

(defun emcp--dedup-by-name (symbols properties)
  "Deduplicate SYMBOLS by MCP name, keeping the last occurrence.

PROPERTIES is a symbol property or list of properties to check.  Each
property holds a plist with a \\=':name key."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (sym (reverse symbols))
      (let ((name (cl-some (lambda (prop)
                             (plist-get (get sym prop) :name))
                           properties)))
        (unless (gethash name seen)
          (puthash name t seen)
          (push sym result))))
    result))

(defun emcp--first-sentence (string)
  "Extract the first sentence from STRING.

Falls back to the first line if no period is found."
  (replace-regexp-in-string
   "\n" " "
   (if (string-match "\\`\\([^.]*\\)\\." string)
       (match-string 1 string)
     (car (split-string string "\n")))))

(defun emcp--read-profile (prompt profiles)
  "Read a profile name from PROFILES with PROMPT.

PROFILES is a list of profile symbols as used as keys in
`emcp-profiles' for the user to choose from."
  (let ((candidates (mapcar #'symbol-name profiles)))
    (cl-labels ((annotate (cand)
                  (when-let* ((desc (plist-get (alist-get (intern cand) emcp-profiles)
                                               :description)))
                    (format "  %s" (emcp--first-sentence desc))))
                (collection (string pred action)
                  (if (eq action 'metadata)
                      `(metadata (annotation-function . ,#'annotate))
                    (complete-with-action action candidates string pred))))
      (intern (completing-read prompt #'collection nil t)))))

(defun emcp-server-url (server)
  "Return the HTTP URL of SERVER."
  (let ((transport (emcp--server-transport server)))
    (http-server-url transport (process-get transport :emcp-path))))

(defun emcp-start (profile)
  "Start an MCP server.

PROFILE is a symbol naming one of the profiles in `emcp-profiles'.  If
`emcp-default-profile' is nil or when called with \\[universal-argument]
prefix, read PROFILE from the minibuffer.  Otherwise, PROFILE will be
`emcp-default-profile'.

When called interactively, show the URL of the server as a message and
add it to the kill ring.

In the end, return the started server.

When a server for PROFILE is already running, use that instead of
starting another one."
  (interactive (list (and (not current-prefix-arg) emcp-default-profile)))
  (unless profile
    (setq profile (emcp--read-profile "Profile: " (mapcar #'car emcp-profiles))))
  (if-let* ((existing (alist-get profile emcp--servers)))
      (progn
        (when (called-interactively-p 'any)
          (let ((url (emcp-server-url existing)))
            (kill-new url)
            (message "Already running at %s (in kill-ring)" url)))
        existing)
    (let* ((resolved (emcp--resolve-profile (alist-get profile emcp-profiles)))
           (server (emcp--server-build :name (format "emcp-%s" profile)
                                       :prompts (plist-get resolved :prompts)
                                       :resources (plist-get resolved :resources)
                                       :tools (plist-get resolved :tools))))
      (emcp-http--start-transport server)
      (push (cons profile server) emcp--servers)
      (when (called-interactively-p 'any)
        (let ((url (emcp-server-url server)))
          (kill-new url)
          (message "Started EMCP at %s (in kill-ring)" url)))
      server)))

(defun emcp-stop (profile)
  "Stop a running MCP server.

PROFILE is a symbol naming one of the running profiles.  When called
interactively or when PROFILE is nil, read it from the minibuffer, unless
there is only a single running profile."
  (interactive (list nil))
  (unless profile
    (setq profile (pcase (length emcp--servers)
                    (0 (user-error "No running servers"))
                    (1 (caar emcp--servers))
                    (_ (emcp--read-profile "Profile: " (mapcar #'car emcp--servers))))))
  (if-let* ((entry (assq profile emcp--servers)))
      (progn
        (emcp-http--stop-transport (cdr entry))
        (emcp--server-stop (cdr entry))
        (setq emcp--servers (assq-delete-all profile emcp--servers))
        (when (called-interactively-p 'any)
          (message "Stopped %s" profile)))
    (user-error "No running server for profile: %s" profile)))

(defun emcp-restart (profile)
  "Restart a running MCP server.

PROFILE is a symbol naming one of the running profiles.  When called
interactively or when PROFILE is nil, read it from the minibuffer, unless
there is only a single running profile."
  (interactive (list nil))
  (unless profile
    (setq profile (pcase (length emcp--servers)
                    (0 (user-error "No running servers"))
                    (1 (caar emcp--servers))
                    (_ (emcp--read-profile "Profile: " (mapcar #'car emcp--servers))))))
  (if-let* ((running (alist-get profile emcp--servers)))
      (progn
        (emcp-http--stop-transport running)
        (emcp--server-stop running)
        (setq emcp--servers (assq-delete-all profile emcp--servers))
        (let* ((resolved (emcp--resolve-profile (alist-get profile emcp-profiles)))
               (server (emcp--server-build :name (format "emcp-%s" profile)
                                           :prompts (plist-get resolved :prompts)
                                           :resources (plist-get resolved :resources)
                                           :tools (plist-get resolved :tools))))
          (emcp-http--start-transport server)
          (push (cons profile server) emcp--servers)
          (when (called-interactively-p 'any)
            (message "Restarted %s" profile))
          server))
    (user-error "%s is not running" profile)))

(defun emcp-reload (profile)
  "Reload PROFILE without restarting its server.

When called interactively or when PROFILE is nil, read it from the
minibuffer, unless there is only a single running profile."
  (interactive (list nil))
  (unless profile
    (setq profile (pcase (length emcp--servers)
                    (0 (user-error "No running servers"))
                    (1 (caar emcp--servers))
                    (_ (emcp--read-profile "Profile: " (mapcar #'car emcp--servers))))))
  (if-let* ((server (alist-get profile emcp--servers)))
      (let* ((resolved (emcp--resolve-profile (alist-get profile emcp-profiles))))
        (emcp--server-reload server
                             :prompts (plist-get resolved :prompts)
                             :resources (plist-get resolved :resources)
                             :tools (plist-get resolved :tools))
        (when (called-interactively-p 'any)
          (message "Reloaded %s" profile))
        server)
    (user-error "%s is not running" profile)))

(provide 'emcp)
;;; emcp.el ends here
