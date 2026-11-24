;;; emcp-core.el --- Core facilities of EMCP -*- lexical-binding: t -*-

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

;; This file contains logging, JSON-RPC and MCP protocol utilities,
;; the MCP server struct, the per-request context, the handlers for
;; MCP requests and the primitive-defining macros.

;;; Code:

(require 'cl-lib)
(require 'cl-print)
(require 'rx)
(require 'seq)

(require 'emcp-uri)

(define-error 'emcp-error "emcp")

;; Signaled with data (CODE MESSAGE DATA) for the JSON-RPC error response
(define-error 'emcp-invalid-request "Invalid MCP request" 'emcp-error)

;; Signaled by resource bodies to report a missing resource
(define-error 'emcp-resource-not-found "Resource not found" 'emcp-error)

(defgroup emcp ()
  "An Emacs MCP server."
  :group 'environment)

(defcustom emcp-instructions "Inspect and control the user's running Emacs.

Use this when you are
- answering questions about Emacs,
- debugging Emacs,
- programming or planning Emacs Lisp,
- looking up Emacs Lisp files,
- or controlling an Emacs instance.

Prefer this server over file operations or shell commands when working in an Emacs context."
  "Instructions for clients discovering the server.

Should describe the overall purpose of the server."
  :group 'emcp
  :type 'string)

(defconst emcp-supported-versions '("2026-07-28")
  "Versions of the MCP spec supported by EMCP.")

(defcustom emcp-list-ttl-ms 300000
  "Milliseconds for which clients may cache lists and server/discover results.

This is the cache hint for the results of tools/list, prompts/list,
resources/list, resources/templates/list and server/discover.  Clients
that subscribe to list changes refetch lists after `emcp-reload' anyway,
so the hint can be long."
  :group 'emcp
  :type 'natnum)

;;; Logging

(eval-and-compile
  ;; Has to be available at compile time for emcp--log
  (defconst emcp-log-levels '(trace debug info warning error)
    "Possible log levels from most to least chatty.")
  (defconst emcp--log-label-width 6
    "Width of the client label at the start of each log line.")
  (defconst emcp--log-request-width 4
    "Minimum width of the request number column in log lines."))

(defcustom emcp-log-level 'info
  "Current log level."
  :group 'emcp
  :type `(choice ,@(mapcar (lambda (l) `(const ,l)) emcp-log-levels)))

(defmacro emcp--log (server context &rest clauses)
  "Log to the log-buffer of SERVER.

CONTEXT is the `emcp-context' of the request being handled or nil.

CLAUSES are evaluated in turn, so one call can log at multiple log
levels.

Each clause is (LEVEL BODY...).  If `emcp-log-level' is at most LEVEL,
evaluate BODY for a message.  If the message is nil, nothing is logged."
  (declare (indent 2) (debug (form form &rest (symbolp body))))
  (let ((log-buffer (gensym "log-buffer"))
        (point-at-max (gensym "point-at-max"))
        (level-pos (gensym "level-pos"))
        (context-var (gensym "context"))
        (message (gensym "message")))
    `(when-let* ((,log-buffer (emcp--server-log-buffer ,server))
                 ((buffer-live-p ,log-buffer)))
       (with-current-buffer ,log-buffer
         (let* ((inhibit-read-only t)
                (,point-at-max (equal (point) (point-max)))
                (,level-pos (seq-position emcp-log-levels emcp-log-level))
                (,context-var ,context))
           (save-excursion
             (goto-char (point-max))
             ,@(mapcar
                (lambda (clause)
                  (let* ((level (car clause))
                         (level-name (format "%-5s" (upcase (symbol-name level))))
                         (body (cdr clause)))
                    `(when-let* (((<= ,level-pos ,(seq-position emcp-log-levels level)))
                                 (,message (progn ,@body)))
                       (let ((prefix (format "%s %s %s "
                                             (format-time-string "%Y/%m/%d %T.%3N")
                                             ,level-name
                                             (emcp--context-log-columns ,context-var))))
                         (insert (emcp--prefix-lines prefix ,message) "\n")))))
                clauses))
           (when ,point-at-max
             (goto-char (point-max))))))))

(defvar emcp--log-escape-placeholder "×"
  "Replacement for non-ASCII characters in logs.")

(defconst emcp--log-escape-regexp
  ;; Printable ASCII (space to tilde) plus line breaks and tabs
  (rx (not (any ?\n ?\r ?\t (?\x20 . ?\x7E))))
  "Matches any character that should not be printed in the log.")

(defun emcp--log-escape (message)
  "Escape MESSAGE for logging."
  (replace-regexp-in-string emcp--log-escape-regexp
                            emcp--log-escape-placeholder
                            message t t))

(cl-defun emcp--log-fill (message &key (width 60))
  "Fill MESSAGE as a text block of width WIDTH.

Useful for logging of long messages and binary blobs."
  (replace-regexp-in-string (rx-to-string `(repeat ,width not-newline))
                            "\\&\n" message t))

(defun emcp--prefix-lines (prefix str)
  "Prepend PREFIX to the lines of STR."
  (concat prefix (replace-regexp-in-string "\n" (concat "\n" prefix) str)))

(defun emcp--to-alist (object)
  "Recursively convert hash tables in OBJECT to alists.

Keys are interned so that the output matches handwritten alist style."
  (cond
   ((hash-table-p object)
    (let (result)
      (maphash (lambda (k v)
                 (push (cons (intern k) (emcp--to-alist v)) result))
               object)
      (nreverse result)))
   ((vectorp object)
    (cl-map 'vector #'emcp--to-alist object))
   (t object)))

(defun emcp--log-pp (object)
  "Pretty-print OBJECT."
  ;; `pp-to-string' adds a newline after the object
  (string-trim (pp-to-string object)))

;;; JSON-RPC 2.0 utilities

(defconst emcp--jsonrpc-parse-error -32700
  "Error code for invalid JSON.")

(defconst emcp--jsonrpc-invalid-request -32600
  "Error code for an invalid request.")

(defconst emcp--jsonrpc-method-not-found -32601
  "Error code for method not found.")

(defconst emcp--jsonrpc-invalid-params -32602
  "Error code for invalid method parameters.")

(defconst emcp--jsonrpc-internal-error -32603
  "Error code for internal error.")

(defun emcp--jsonrpc-type (object)
  "Determine the type of the JSON-RPC object OBJECT.

Return \\='request if OBJECT is a JSON-RPC request, \\='result for a
successful response, \\='error for an error response, \\='notification
for a notification and nil otherwise."
  (and (hash-table-p object)
       (equal (gethash "jsonrpc" object) "2.0")
       (if (stringp (gethash "method" object))
           (if-let* ((id (gethash "id" object)))
               (when (or (stringp id) (numberp id))
                 'request)
             'notification)
         (when-let* ((id (gethash "id" object))
                     ((or (stringp id) (numberp id))))
           (cond
            ((gethash "result" object)
             'result)
            ((and-let* ((error (gethash "error" object))
                        ((integerp (gethash "code" error)))
                        ((stringp (gethash "message" error)))))
             'error))))))

(defun emcp--jsonrpc-notification (method params)
  "Build a notification for METHOD and PARAMS."
  `((jsonrpc . "2.0")
    (method . ,method)
    ,@(when params `((params . ,params)))))

(defun emcp--jsonrpc-result (request result)
  "Build a successful response to REQUEST with RESULT data."
  `((jsonrpc . "2.0")
    (id . ,(gethash "id" request))
    (result . ,result)))

(defun emcp--jsonrpc-error (request code message &optional data)
  "Build an error response to REQUEST.

If REQUEST is nil, the response has no id.  CODE is an integer error
code, MESSAGE is a short human-readable description and DATA is
arbitrary data related to the error."
  `((jsonrpc . "2.0")
    ,@(when request `((id . ,(gethash "id" request))))
    (error . ((code . ,code)
              (message . ,message)
              ,@(when data `((data . ,data)))))))

;;; MCP protocol

(defconst emcp--mcp-header-mismatch -32020
  "Error code for HTTP headers that are missing or do not match the body.")

(defconst emcp--mcp-unsupported-protocol-version -32022
  "Error code for a request with an unsupported protocol version.")

(defconst emcp--mcp-log-level-names
  '("debug" "info" "notice" "warning" "error" "critical" "alert" "emergency")
  "The log levels defined by MCP.

See https://modelcontextprotocol.io/specification/2026-07-28/server/utilities/logging")

(defconst emcp--mcp-server-info
  '((name . "EMCP")
    (title . "EMCP - An MCP server for Emacs")
    (version . "0.1.0")
    (description . "Lets your agent talk to Emacs")
    (websiteUrl . "https://codeberg.org/martenlienen/emcp"))
  "The server info that identifies EMCP in the _meta of every result.

Its fields follow the Implementation type of the MCP schema.")

(defconst emcp--mcp-server-icons
  (when-let* ((file (or load-file-name buffer-file-name))
              (path (expand-file-name "emacs.svg" (file-name-directory file)))
              ((file-readable-p path)))
    (let ((data (with-temp-buffer
                  (insert-file-contents-literally path)
                  (buffer-string))))
      `[((mimeType . "image/svg+xml")
         (sizes . ["any"])
         (src . ,(concat "data:image/svg+xml;base64," (base64-encode-string data t))))]))
  "The icons of EMCP, or nil if the icon file is missing.

Only server/discover sends them, because the base64 data is too large to
include in every result.")

(defconst emcp--mcp-subscription-notifications
  '(("toolsListChanged" . "notifications/tools/list_changed")
    ("promptsListChanged" . "notifications/prompts/list_changed")
    ("resourcesListChanged" . "notifications/resources/list_changed"))
  "The notification types that clients can subscribe to and their methods.

Each entry maps a field of the notifications filter of a
subscriptions/listen request to the notification that EMCP sends for
it.  EMCP does not support resources/subscribe, so it ignores the filter
field \"resourceSubscriptions\".  See
https://modelcontextprotocol.io/specification/2026-07-28/basic/patterns/subscriptions")

(defun emcp--mcp-result (request result)
  "Build a successful MCP response to REQUEST with RESULT data.

Mark RESULT as complete and add the server info to its _meta entries,
unless RESULT sets it itself."
  (let ((meta (alist-get '_meta result)))
    (emcp--jsonrpc-result
     request
     `((resultType . "complete")
       ,@(assq-delete-all '_meta (copy-sequence result))
       (_meta . (,@meta
                 ,@(unless (assq 'io.modelcontextprotocol/serverInfo meta)
                     `((io.modelcontextprotocol/serverInfo
                        . ,emcp--mcp-server-info)))))))))

(defun emcp--mcp-cacheable (result ttl-ms scope)
  "Add the cache hints TTL-MS and SCOPE to RESULT.

SCOPE is \"public\" or \"private\", see
https://modelcontextprotocol.io/specification/2026-07-28/server/utilities/caching"
  `(,@result (ttlMs . ,ttl-ms) (cacheScope . ,scope)))

;;; MCP Server

(cl-defstruct (emcp--server (:constructor emcp--make-server)
                            (:copier nil))
  "An MCP server."
  (name "emcp" :type 'string :documentation "Server name")
  (log-buffer (with-current-buffer (generate-new-buffer (format "*%s*" name))
                (setq buffer-read-only t)
                (current-buffer)))
  transport
  (prompts (make-hash-table :test 'equal) :type 'hash-table
           :documentation "Available prompt templates.")
  (resources (make-hash-table :test 'equal) :type 'hash-table
             :documentation "Available resources.")
  (resource-templates (make-hash-table :test 'equal) :type 'hash-table
                      :documentation "Available resource templates.")
  (tools (make-hash-table :test 'equal) :type 'hash-table
         :documentation "Available tools.")
  (modes nil :type 'list
         :documentation "Plist of tool modes, e.g. :emcp-tools-eval-mode.")
  (subscriptions nil :type 'list
                 :documentation "Open `emcp--subscription's.")
  (request-count 0 :type 'integer
                 :documentation "Number of requests so far, for logging."))

(cl-defun emcp--server-build (&key name prompts resources tools)
  "Build a new MCP server from lists of primitive symbols.

NAME is the server name.

PROMPTS, RESOURCES, and TOOLS are lists of symbols defined via
`emcp-defprompt', `emcp-defresource' and `emcp-deftool'."
  (let ((server (emcp--make-server :name name)))
    (emcp--server-reload server
                         :prompts prompts
                         :resources resources
                         :tools tools)
    server))

(defun emcp--server-destroy (server)
  "Release the resources of SERVER, i.e. its log buffer."
  (kill-buffer (emcp--server-log-buffer server)))

(cl-defun emcp--server-reload (server &key prompts resources tools)
  "Reload the components of SERVER.

PROMPTS, RESOURCES, and TOOLS are lists of symbols defined via
`emcp-defprompt', `emcp-defresource' and `emcp-deftool'."

  (let ((server-prompts (emcp--server-prompts server)))
    (clrhash server-prompts)
    (dolist (sym prompts)
      (puthash (plist-get (get sym 'emcp-prompt) :name) sym server-prompts)))

  (let ((server-resources (emcp--server-resources server))
        (server-templates (emcp--server-resource-templates server)))
    (clrhash server-resources)
    (clrhash server-templates)
    (dolist (sym resources)
      (if-let* ((template (get sym 'emcp-resource-template)))
          (puthash (plist-get template :name) sym server-templates)
        (puthash (plist-get (get sym 'emcp-resource) :name) sym server-resources))))

  (let ((server-tools (emcp--server-tools server)))
    (clrhash server-tools)
    (dolist (sym tools)
      (puthash (plist-get (get sym 'emcp-tool) :name) sym server-tools)))

  (pcase-dolist (`(,_ . ,method) emcp--mcp-subscription-notifications)
    (emcp--server-notify-list-changed server method)))

(defun emcp--server-mode (server key)
  "Return the tool mode stored under KEY in SERVER, or nil if unset.

KEY is a keyword such as :emcp-tools-eval-mode.  The mode applies to all
clients of SERVER.  Set it with `setf'."
  (plist-get (emcp--server-modes server) key))

(gv-define-setter emcp--server-mode (mode server key)
  `(setf (plist-get (emcp--server-modes ,server) ,key) ,mode))

(defun emcp--server-capabilities (_server)
  "Describe MCP server capabilities of SERVER."
  '((prompts . ((listChanged . t)))
    (resources . ((listChanged . t)))
    (tools . ((listChanged . t)))))

;;; Request context

(cl-defstruct (emcp-context (:constructor emcp--make-context)
                            (:copier nil))
  "The context of a single MCP request."
  (server nil :documentation "The `emcp--server' handling the request.")
  (protocol-version nil :documentation "The MCP protocol version string.")
  (client-info nil :documentation "The clientInfo object as a hash table, or nil.")
  (client-capabilities nil :documentation "The client capabilities as a hash table.")
  (request-id nil :documentation "The JSON-RPC request id, or nil.")
  (number nil :documentation "The number of the request on its server, or nil.")
  (cancelled nil :documentation "Non-nil once the request has been cancelled.")
  (cancel-hooks nil :documentation "(FUNCTION . ARGS) to call on cancellation, newest first."))

(defun emcp-context-on-cancel (context function &rest args)
  "Call FUNCTION with ARGS when the request of CONTEXT is cancelled.

A client cancels a request, for example, by closing the connection
before the response.  Use this in async bodies to stop work that is no
longer needed.  If the request has already been cancelled, call
FUNCTION right away."
  (if (emcp-context-cancelled context)
      (apply function args)
    (push (cons function args) (emcp-context-cancel-hooks context))))

(defun emcp--context-cancel (context)
  "Cancel the request of CONTEXT.

Run its cancel hooks unless it was already cancelled.  Responses to it
will not be sent anymore."
  (unless (emcp-context-cancelled context)
    (let ((hooks (reverse (emcp-context-cancel-hooks context))))
      (setf (emcp-context-cancelled context) t
            (emcp-context-cancel-hooks context) nil)
      (emcp--log (emcp-context-server context) context
        (info "Request cancelled"))
      (dolist (hook hooks)
        ;; One failing hook must not keep the others from cleaning up
        (condition-case err
            (apply (car hook) (cdr hook))
          (error
           (emcp--log (emcp-context-server context) context
             (error (format "Cancel hook %s failed: %s"
                            (emcp--log-escape (cl-prin1-to-string hook))
                            (emcp--log-escape (error-message-string err)))))))))))

(defun emcp--request-context (server request)
  "Return the `emcp-context' for the JSON-RPC REQUEST to SERVER.

If the _meta of REQUEST is invalid, signal `emcp-invalid-request' with
the JSON-RPC error CODE, MESSAGE and DATA."
  (let* ((params (gethash "params" request))
         (meta (and (hash-table-p params) (gethash "_meta" params)))
         (field (lambda (key)
                  (and (hash-table-p meta)
                       (gethash (concat "io.modelcontextprotocol/" key) meta))))
         (version (funcall field "protocolVersion"))
         (capabilities (funcall field "clientCapabilities"))
         (client-info (funcall field "clientInfo"))
         (log-level (funcall field "logLevel")))
    (cl-flet ((fail (code message &optional data)
                (signal 'emcp-invalid-request (list code message data))))
      (cond
       ((not (stringp version))
        (fail emcp--jsonrpc-invalid-params
              "Missing or invalid _meta field io.modelcontextprotocol/protocolVersion"))
       ((not (member version emcp-supported-versions))
        (fail emcp--mcp-unsupported-protocol-version "Unsupported protocol version"
              `((supported . ,(vconcat emcp-supported-versions))
                (requested . ,version))))
       ((not (hash-table-p capabilities))
        (fail emcp--jsonrpc-invalid-params
              "Missing or invalid _meta field io.modelcontextprotocol/clientCapabilities"))
       ((and client-info
             (not (and (hash-table-p client-info)
                       (stringp (gethash "name" client-info))
                       (stringp (gethash "version" client-info))
                       (stringp (gethash "title" client-info "")))))
        (fail emcp--jsonrpc-invalid-params
              "Invalid _meta field io.modelcontextprotocol/clientInfo"))
       ((and log-level (not (member log-level emcp--mcp-log-level-names)))
        (fail emcp--jsonrpc-invalid-params
              (format "Invalid log level in _meta: %S" log-level)))
       (t
        (emcp--make-context :server server
                            :protocol-version version
                            :client-info client-info
                            :client-capabilities capabilities
                            :request-id (gethash "id" request)
                            ;; Client ids are only unique per client
                            :number (cl-incf (emcp--server-request-count server))))))))

(defun emcp--context-client-info-field (context field)
  "Return the non-empty string FIELD of CONTEXT's clientInfo, or nil."
  (when-let* ((info (emcp-context-client-info context))
              (value (gethash field info))
              ((not (string-empty-p value))))
    value))

(defun emcp--sanitize-label (label)
  "Make the client-controlled LABEL safe to display on a single line."
  (apply #'string
         (mapcar (lambda (char)
                   (let ((category (symbol-name (get-char-code-property
                                                 char 'general-category))))
                     ;; Keep letters, marks, numbers, punctuation,
                     ;; symbols and spaces and replace every other
                     ;; character, e.g. control characters, bidi
                     ;; overrides, line breaks and unassigned code
                     ;; points, with `emcp--log-escape-placeholder'.
                     (if (or (memq (aref category 0) '(?L ?M ?N ?P ?S))
                             (equal category "Zs"))
                         char
                       (string-to-char emcp--log-escape-placeholder))))
                 label)))

(defun emcp--context-label (context)
  "Format a short, human-readable label for CONTEXT."
  (format "%s (%s)"
          (if-let* ((label (or (emcp--context-client-info-field context "title")
                               (emcp--context-client-info-field context "name"))))
              (emcp--sanitize-label label)
            "Unknown client")
          (emcp--server-name (emcp-context-server context))))

(defun emcp--context-log-columns (context)
  "Return the client label and request number of CONTEXT for logging.

The label is the clientInfo name.  The request number is right-aligned.
Unknown values and a nil CONTEXT show as dashes."
  (format (format "%%s %%%ds" emcp--log-request-width)
          (if context
              (emcp--context-log-label context)
            (make-string emcp--log-label-width ?-))
          (if context
              (emcp-context-number context)
            (make-string emcp--log-request-width ?-))))

(defun emcp--context-log-label (context)
  "Return the client label of CONTEXT for logging.

The label is the clientInfo name or dashes if the name is unknown."
  (if-let* ((name (emcp--context-client-info-field context "name")))
      (truncate-string-to-width
       ;; Replace anything outside printable ASCII (space to tilde).  Unlike
       ;; `emcp--log-escape', this includes newlines and tabs, which would break
       ;; the line or the fixed width.
       (replace-regexp-in-string (rx (not (any (?\x20 . ?\x7E))))
                                 emcp--log-escape-placeholder name t t)
       emcp--log-label-width nil ?\s)
    (make-string emcp--log-label-width ?-)))

;;; Subscriptions

(cl-defstruct (emcp--subscription (:constructor emcp--make-subscription)
                                  (:copier nil))
  "A subscriptions/listen stream."
  (request nil :documentation "The subscriptions/listen request.")
  (context nil :documentation "The `emcp-context' of the request.")
  (notifications nil :documentation "The fields of `emcp--mcp-subscription-notifications'
that the client subscribed to.")
  (send nil :documentation "Sends a message, see `emcp--server-subscribe'."))

(defun emcp--subscription-requested-notifications (request)
  "Return the notification types that REQUEST asks for and EMCP can send.

REQUEST is a subscriptions/listen request.  The notification types are
fields of `emcp--mcp-subscription-notifications' that the notifications
filter of REQUEST sets to true.  If REQUEST has no filter, signal
`emcp-invalid-request' with the JSON-RPC error CODE, MESSAGE and DATA."
  (let* ((params (gethash "params" request))
         (filter (and (hash-table-p params) (gethash "notifications" params))))
    (unless (hash-table-p filter)
      (signal 'emcp-invalid-request
              (list emcp--jsonrpc-invalid-params "Missing or invalid notifications filter" nil)))
    (cl-loop for (field . _) in emcp--mcp-subscription-notifications
             when (eq (gethash field filter) t)
             collect field)))

(defun emcp--subscription-id (subscription)
  "Return the id of SUBSCRIPTION, i.e. the id of its subscriptions/listen request."
  (gethash "id" (emcp--subscription-request subscription)))

(defun emcp--subscription-meta (subscription)
  "Return the _meta entries that tag messages on SUBSCRIPTION."
  `((io.modelcontextprotocol/subscriptionId . ,(emcp--subscription-id subscription))))

(defun emcp--server-subscription-notify
    (server subscription method &optional params final)
  "Send the notification METHOD with PARAMS on SUBSCRIPTION of SERVER.

If FINAL is non-nil, end the stream of SUBSCRIPTION afterwards."
  (emcp--log server (emcp--subscription-context subscription)
    (info (format "< Notification %s" method)))
  (funcall (emcp--subscription-send subscription)
           (emcp--jsonrpc-notification
            method `((_meta . ,(emcp--subscription-meta subscription)) ,@params))
           final))

(defun emcp--server-subscribe (server context request notifications send)
  "Register a subscription on SERVER and return it.

REQUEST is the subscriptions/listen request, CONTEXT its `emcp-context'
and NOTIFICATIONS the value of `emcp--subscription-requested-notifications'
for it.  SEND sends messages on the stream of the subscription and is
called as (funcall SEND MESSAGE &optional FINAL) with a JSON-RPC MESSAGE
alist.  If FINAL is non-nil, the stream ends after MESSAGE.

The caller must send the acknowledgment with
`emcp--server-acknowledge-subscription' before anything else."
  (emcp--log server context
    (info (format "> Request subscriptions/listen (id %s)" (gethash "id" request)))
    (debug (emcp--prefix-lines "> " (emcp--log-pp (emcp--to-alist (gethash "params" request))))))
  (let ((subscription (emcp--make-subscription :request request
                                               :context context
                                               :notifications notifications
                                               :send send)))
    (push subscription (emcp--server-subscriptions server))
    subscription))

(defun emcp--server-acknowledge-subscription (server subscription)
  "Acknowledge SUBSCRIPTION on SERVER with the notification types it receives."
  (emcp--server-subscription-notify
   server subscription "notifications/subscriptions/acknowledged"
   `((notifications . ,(mapcar (lambda (field) (cons (intern field) t))
                               (emcp--subscription-notifications subscription))))))

(defun emcp--server-unsubscribe (server subscription)
  "Remove SUBSCRIPTION from SERVER after its client closed the stream."
  (setf (emcp--server-subscriptions server)
        (delq subscription (emcp--server-subscriptions server)))
  (emcp--log server (emcp--subscription-context subscription)
    (info "Subscription closed by the client")))

(defun emcp--server-close-subscriptions (server)
  "End all subscriptions on SERVER, e.g. because it shuts down.

Send a final notifications/cancelled notification on each stream that
references its subscriptions/listen request."
  (let ((subscriptions (emcp--server-subscriptions server)))
    (setf (emcp--server-subscriptions server) nil)
    (dolist (subscription subscriptions)
      (let ((context (emcp--subscription-context subscription))
            (id (emcp--subscription-id subscription)))
        (emcp--log server context
          (info "Subscription ended by the server"))
        ;; One failing stream must not keep the others open
        (condition-case err
            ;; The cancellation pattern of the spec requires this
            ;; notification (MUST), which overrides the complete result
            ;; that the subscriptions pattern recommends (SHOULD)
            (emcp--server-subscription-notify
             server subscription "notifications/cancelled"
             `((requestId . ,id) (reason . "The server is shutting down"))
             t)
          (error
           (emcp--log server context
             (error (format "Failed to end subscription: %s"
                            (error-message-string err))))))))))

(defun emcp--server-notify-list-changed (server method)
  "Send the list-changed notification METHOD to the subscriptions on SERVER.

Only subscriptions that requested METHOD receive it.

METHOD is one of the notifications in `emcp--mcp-subscription-notifications'."
  (let ((field (car (rassoc method emcp--mcp-subscription-notifications))))
    ;; Iterate over a copy, because sending can unsubscribe a closed
    ;; stream
    (dolist (subscription (copy-sequence (emcp--server-subscriptions server)))
      (when (member field (emcp--subscription-notifications subscription))
        (emcp--server-subscription-notify server subscription method)))))

;;; Request handling

(defun emcp--server-on-request (server context request send-response)
  "Handle the MCP JSON-RPC2.0 request REQUEST on SERVER with CONTEXT.

CONTEXT is the value of `emcp--request-context' for REQUEST.
SEND-RESPONSE is called with the response alist, unless the request
has been cancelled via `emcp--context-cancel' by then."
  (emcp--log server context
    (info
     (let* ((method (gethash "method" request))
            (params (gethash "params" request))
            (detail (pcase method
                      ((or "tools/call" "prompts/get") (gethash "name" params))
                      ("resources/read" (gethash "uri" params)))))
       (format "> Request %s%s (id %s)"
               method
               (if detail (format " %s" detail) "")
               (gethash "id" request))))
    (debug (if-let* ((params (gethash "params" request)))
               (emcp--prefix-lines "> " (emcp--log-pp (emcp--to-alist params)))
             "No parameters")))
  (cl-flet ((log-and-send (response)
              (let ((cancelled (emcp-context-cancelled context)))
                (emcp--log server context
                  (info (format "%s %s"
                                ;; The spec forbids sending it
                                (if cancelled "Dropping response" "Response")
                                (if (alist-get 'error response) "error" "ok")))
                  (debug (emcp--prefix-lines "< " (emcp--log-pp
                                                   (or (alist-get 'result response)
                                                       (alist-get 'error response))))))
                (unless cancelled
                  (condition-case err
                      (funcall send-response response)
                    (error
                     (emcp--log server context
                       (error (format "Failed to send response: %s"
                                      (error-message-string err))))
                     ;; Answer the request anyway, e.g. if RESPONSE is
                     ;; not valid JSON
                     (condition-case fallback-err
                         (funcall send-response (emcp--jsonrpc-error
                                                 request
                                                 emcp--jsonrpc-internal-error
                                                 "Failed to send response"))
                       (error
                        (emcp--log server context
                          (error (format "Failed to send error response: %s"
                                         (error-message-string fallback-err))))))
                     ;; Let the caller know about the original failure
                     (signal (car err) (cdr err))))))))
    (handler-bind
        ;; Last resort response in case an error interrupts request handling
        ((error
          (lambda (err)
            (emcp--log server context
              (error (format "Error during request handling:\n%s"
                             (emcp--prefix-lines "| " (error-message-string err)))))
            (ignore-errors
              (log-and-send (emcp--jsonrpc-error
                             request
                             emcp--jsonrpc-internal-error
                             "Internal server error"))))))
      (pcase (gethash "method" request)
        ("server/discover"
         (emcp--server-request--discover server context request #'log-and-send))
        ("prompts/list"
         (emcp--server-request--prompts-list server context request #'log-and-send))
        ("prompts/get"
         (emcp--server-request--prompts-get server context request #'log-and-send))
        ("resources/list"
         (emcp--server-request--resources-list server context request #'log-and-send))
        ("resources/templates/list"
         (emcp--server-request--resources-templates-list server context request #'log-and-send))
        ("resources/read"
         (emcp--server-request--resources-read server context request #'log-and-send))
        ("tools/list"
         (emcp--server-request--tools-list server context request #'log-and-send))
        ("tools/call"
         (emcp--server-request--tools-call server context request #'log-and-send))
        (_ (log-and-send (emcp--jsonrpc-error
                          request
                          emcp--jsonrpc-method-not-found
                          "Method not found")))))))

(defun emcp--server-request--discover (server _context request send-response)
  "Describe SERVER and its capabilities.

SEND-RESPONSE is called with the response to REQUEST."
  (funcall send-response
           (emcp--mcp-result
            request
            (emcp--mcp-cacheable
             `((supportedVersions . ,(vconcat emcp-supported-versions))
               (capabilities . ,(emcp--server-capabilities server))
               (instructions . ,emcp-instructions)
               (_meta . ((io.modelcontextprotocol/serverInfo
                          . (,@emcp--mcp-server-info
                             ,@(when emcp--mcp-server-icons
                                 `((icons . ,emcp--mcp-server-icons))))))))
             emcp-list-ttl-ms "public"))))

(defun emcp--server-request--prompts-list (server _context request send-response)
  "List all prompt definitions on SERVER.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((prompts (emcp--server-prompts server))
         (descriptions (cl-loop for prompt being the hash-values of prompts
                                collect (plist-get (get prompt 'emcp-prompt) :metadata))))
    (funcall send-response
             (emcp--mcp-result
              request (emcp--mcp-cacheable `((prompts . ,(vconcat descriptions)))
                                           emcp-list-ttl-ms "public")))))

(defun emcp--request-arguments (params)
  "Return the arguments of the tools/call or prompts/get PARAMS.

Missing arguments, which the spec allows, yield an empty hash table.
Return nil if the arguments are not an object."
  (let ((args (and (hash-table-p params) (gethash "arguments" params))))
    (cond ((null args) (make-hash-table :test #'equal))
          ((hash-table-p args) args))))

(defun emcp--server-request--prompts-get (server context request send-response)
  "Execute a prompt on SERVER with request CONTEXT.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((params (gethash "params" request))
         (name (and (hash-table-p params) (gethash "name" params)))
         (prompt (gethash name (emcp--server-prompts server)))
         (args (emcp--request-arguments params)))
    (cl-flet ((send-result (result)
                (funcall send-response (emcp--mcp-result request result)))
              (send-error (code message &optional data)
                (funcall send-response (emcp--jsonrpc-error request code message data))))
      (cond
       ((not prompt)
        (send-error emcp--jsonrpc-invalid-params (format "Unknown prompt: %s" name)))
       ;; The schema requires an object, so anything else is malformed
       ((not args)
        (send-error emcp--jsonrpc-invalid-params "Arguments must be an object"))
       (t
        (funcall prompt server context #'send-result #'send-error args))))))

(defun emcp--server-request--resources-list (server _context request send-response)
  "List all static resources on SERVER.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((resources (emcp--server-resources server))
         (descriptions (cl-loop for sym being the hash-values of resources
                                collect (plist-get (get sym 'emcp-resource) :metadata))))
    (funcall send-response
             (emcp--mcp-result
              request (emcp--mcp-cacheable `((resources . ,(vconcat descriptions)))
                                           emcp-list-ttl-ms "public")))))

(defun emcp--server-request--resources-templates-list (server _context request send-response)
  "List all resource templates on SERVER.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((templates (emcp--server-resource-templates server))
         (descriptions (cl-loop for sym being the hash-values of templates
                                collect (plist-get (get sym 'emcp-resource-template) :metadata))))
    (funcall send-response
             (emcp--mcp-result
              request (emcp--mcp-cacheable `((resourceTemplates . ,(vconcat descriptions)))
                                           emcp-list-ttl-ms "public")))))

(defun emcp--server-find-resource-template (server uri)
  "Find a resource template on SERVER matching URI.

Return (SYMBOL . PARAMS) or nil."
  (let (result)
    (maphash
     (lambda (_key sym)
       (unless result
         (when-let* ((template (get sym 'emcp-resource-template))
                     (params (emcp-uri--match uri (plist-get template :match))))
           (setq result (cons sym params)))))
     (emcp--server-resource-templates server))
    result))

(defun emcp--server-request--resources-read (server context request send-response)
  "Read a resource on SERVER in the request CONTEXT.

SEND-RESPONSE is called with the response to REQUEST.  Unknown resources
or resources that signal `emcp-resource-not-found' produce an
invalid-params response."
  (let* ((params (gethash "params" request))
         (uri (gethash "uri" params))
         (static (gethash uri (emcp--server-resources server)))
         (match (unless static (emcp--server-find-resource-template server uri)))
         (sym (or static (car match)))
         (ttl-ms (plist-get (or (get sym 'emcp-resource) (get sym 'emcp-resource-template))
                            :ttl-ms)))
    (cl-flet* ((send-error (code message &optional data)
                 (funcall send-response (emcp--jsonrpc-error request code message data)))
               (send-result (result)
                 (funcall send-response
                          (emcp--mcp-result
                           request (emcp--mcp-cacheable result ttl-ms "private"))))
               (not-found ()
                 (send-error emcp--jsonrpc-invalid-params "Resource not found"
                             `((uri . ,uri)))))
      (condition-case err
          (cond
           ;; Static resource
           (static
            (funcall sym server context #'send-result #'send-error uri))
           ;; Resource template
           (match
            (funcall sym server context #'send-result #'send-error uri (cdr match)))
           (t
            (not-found)))
        (emcp-resource-not-found
         (not-found))
        (error
         (send-error emcp--jsonrpc-internal-error (error-message-string err)))))))

(defun emcp--server-request--tools-list (server _context request send-response)
  "List all tool definitions on SERVER.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((tools (emcp--server-tools server))
         (descriptions (cl-loop for tool being the hash-values of tools
                                collect (plist-get (get tool 'emcp-tool) :metadata))))
    (funcall send-response
             (emcp--mcp-result
              request (emcp--mcp-cacheable `((tools . ,(vconcat descriptions)))
                                           emcp-list-ttl-ms "public")))))

(defun emcp--server-request--tools-call (server context request send-response)
  "Execute a tool on SERVER with request CONTEXT.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((params (gethash "params" request))
         (name (and (hash-table-p params) (gethash "name" params)))
         (tool (gethash name (emcp--server-tools server)))
         (args (emcp--request-arguments params)))
    (cl-flet ((send-result (result)
                (funcall send-response (emcp--mcp-result request result)))
              (send-error (code message &optional data)
                (funcall send-response (emcp--jsonrpc-error request code message data))))
      (cond
       ((not tool)
        (send-error emcp--jsonrpc-invalid-params (format "Unknown tool: %s" name)))
       ;; The schema requires an object, so anything else is malformed
       ((not args)
        (send-error emcp--jsonrpc-invalid-params "Arguments must be an object"))
       (t
        (funcall tool server context #'send-result #'send-error args))))))

;;; Primitive defining macros

(defmacro emcp-deftool (name args docstring &rest body)
  "Define NAME as an MCP tool.

ARGS is a list of argument specifications.  Each element is either a
bare symbol or a list (SYMBOL [DESCRIPTION] [:type TYPE] [:default
DEFAULT]).  Arguments with a :default are optional in MCP; all others
are required.  TYPE ones of the JSON schema types \"string\",
\"number\", \"integer\", \"boolean\", \"null\", \"object\", \"array\".
The default TYPE is \"string\".

The following keyword options may appear before BODY:

 :name MCP tool name (default NAME).
 :title MCP tool title.
 :description MCP tool description (default DOCSTRING).
 :async When non-nil, BODY sends its result or error via the locally
        bound functions `send-result' and `send-error'.  When nil, BODY
        returns a tool result directly.

When the client calls the tool, execute BODY to produce a result.  In
addition to the declared ARGS, the following symbols are bound:

 `server': The MCP server.
 `context': The `emcp-context' of the request.

If :async is nil or not provided, BODY just returns a tool result as
defined in the MCP specification.  If :async is non-nil, the following
functions are bound and BODY has to call exactly one of them once to
send a result or error:

 `send-result' (RESULT): Send the tool RESULT to the client.
 `send-error' (CODE MESSAGE &optional DATA): Signal an error to the
 client with error code CODE, MESSAGE and optional error DATA.

If the client cancels the request, e.g. by closing the connection,
`send-result' and `send-error' do nothing.  BODY can register a
function that stops work that is no longer needed, or check for
cancellation itself:

  (emcp-context-on-cancel context #\\='stop-work an-arg)
  (unless (emcp-context-cancelled context) ...)

RESULT is a tool call result JSON document (an alist) as described in the
spec, see this URL
https://modelcontextprotocol.io/specification/2026-07-28/server/tools"
  (declare (indent 2) (debug (symbolp sexp stringp body)))
  ;; Parse keyword options before body
  (let (mcp-name mcp-title mcp-description async-p)
    (while (keywordp (car body))
      (pcase (pop body)
        (:name (setq mcp-name (pop body)))
        (:title (setq mcp-title (pop body)))
        (:description (setq mcp-description (pop body)))
        (:async (setq async-p (pop body)))))
    (unless mcp-name
      (setq mcp-name (symbol-name name)))
    (unless mcp-description
      (setq mcp-description docstring))
    (let* ((arg-specs
            (cl-loop for arg in args
                     collect (pcase arg
                               ((pred symbolp)
                                (list arg nil nil))
                               (`(,sym ,(and (pred stringp) desc) . ,plist)
                                (list sym desc plist))
                               (`(,sym . ,plist)
                                (list sym nil plist)))))
           (properties
            (cl-loop for (sym desc plist) in arg-specs
                     collect
                     `(,sym . ((type . ,(or (plist-get plist :type) "string"))
                               ,@(when desc `((description . ,desc)))))))
           (required
            (vconcat
             (cl-loop for (sym _ plist) in arg-specs
                      unless (plist-member plist :default)
                      collect (symbol-name sym))))
           (metadata
            `((name . ,mcp-name)
              ,@(when mcp-title `((title . ,mcp-title)))
              (description . ,mcp-description)
              (inputSchema . ((type . "object")
                              (properties . ,properties)
                              ,@(when (> (length required) 0)
                                  `((required . ,required)))))))
           (args-var (gensym "args"))
           (arg-bindings
            (cl-loop for (sym _ plist) in arg-specs
                     collect `(,sym (gethash ,(symbol-name sym) ,args-var
                                             ,(plist-get plist :default)))))
           (send-result-var (gensym "send-result"))
           (send-error-var (gensym "send-error")))
      `(progn
         (put ',name 'emcp-tool '(:name ,mcp-name :metadata ,metadata))
         (defun ,name (server context ,send-result-var ,send-error-var ,args-var)
           ,docstring
           (ignore server context)
           ,(if async-p
                `(cl-flet ((send-result (result)
                             (funcall ,send-result-var result))
                           (send-error (code message &optional data)
                             (funcall ,send-error-var code message data)))
                   (let ,arg-bindings
                     ,@body))
              `(let ,arg-bindings
                 (funcall ,send-result-var (progn ,@body)))))))))

(defmacro emcp-defprompt (name args docstring &rest body)
  "Define NAME as an MCP prompt.

ARGS is a list of argument specifications.  Each element is either a
bare symbol or a list (SYMBOL [DESCRIPTION] [:default DEFAULT]).
Arguments with a :default are optional in MCP; all others are required.
In the BODY, each argument is bound to its value from the MCP request,
falling back to DEFAULT if provided.

The following keyword options may appear before BODY:

 :name MCP prompt name (default NAME).
 :title MCP prompt title.
 :description MCP prompt description (default DOCSTRING).
 :async When non-nil, BODY sends its result or error via the locally
        bound functions `send-result' and `send-error'.  When nil, BODY
        returns a prompt result directly.

When the client requests the prompt, execute BODY to produce a result.
In addition to the declared ARGS, the following symbols are bound:

 `server': The MCP server.
 `context': The `emcp-context' of the request.

If :async is nil or not provided, BODY just returns a prompt result as
defined in the MCP specification.  If :async is non-nil, the following
functions are bound and BODY has to call exactly one of them once to
send a result or error:

 `send-result' (RESULT): Send the prompt RESULT to the client.
 `send-error' (CODE MESSAGE &optional DATA): Signal an error to the
 client with error code CODE, MESSAGE and optional error DATA.

If the client cancels the request, e.g. by closing the connection,
`send-result' and `send-error' do nothing.  BODY can register a
function that stops work that is no longer needed, or check for
cancellation itself:

  (emcp-context-on-cancel context #\\='stop-work an-arg)
  (unless (emcp-context-cancelled context) ...)

RESULT is a prompts/get result JSON document (an alist) as described in the
spec, see this URL
https://modelcontextprotocol.io/specification/2026-07-28/server/prompts"
  (declare (indent 2) (debug (symbolp sexp stringp body)))
  ;; Parse keyword options before body
  (let (mcp-name mcp-title mcp-description async-p)
    (while (keywordp (car body))
      (pcase (pop body)
        (:name (setq mcp-name (pop body)))
        (:title (setq mcp-title (pop body)))
        (:description (setq mcp-description (pop body)))
        (:async (setq async-p (pop body)))))
    (unless mcp-name
      (setq mcp-name (symbol-name name)))
    (unless mcp-description
      (setq mcp-description docstring))
    (let* ((arg-specs
            (cl-loop for arg in args
                     collect (pcase arg
                               ((pred symbolp)
                                (list arg nil nil))
                               (`(,sym ,(and (pred stringp) desc) . ,plist)
                                (list sym desc plist))
                               (`(,sym . ,plist)
                                (list sym nil plist)))))
           (arg-metadata
            (vconcat
             (cl-loop for (sym desc plist) in arg-specs
                      collect `((name . ,(symbol-name sym))
                                ,@(when desc `((description . ,desc)))
                                (required . ,(if (plist-member plist :default) :false t))))))
           (metadata
            `((name . ,mcp-name)
              ,@(when mcp-title `((title . ,mcp-title)))
              (description . ,mcp-description)
              (arguments . ,arg-metadata)))
           (args-var (gensym "args"))
           (arg-bindings
            (cl-loop for (sym _desc plist) in arg-specs
                     collect `(,sym (gethash ,(symbol-name sym) ,args-var
                                             ,(plist-get plist :default))))))
      (let ((send-result-var (gensym "send-result"))
            (send-error-var (gensym "send-error")))
        `(progn
           (put ',name 'emcp-prompt '(:name ,mcp-name :metadata ,metadata))
           (defun ,name (server context ,send-result-var ,send-error-var ,args-var)
             ,docstring
             (ignore server context)
             ,(if async-p
                  `(cl-flet ((send-result (result)
                               (funcall ,send-result-var result))
                             (send-error (code message &optional data)
                               (funcall ,send-error-var code message data)))
                     (let ,arg-bindings
                       ,@body))
                `(let ,arg-bindings
                   (funcall ,send-result-var (progn ,@body))))))))))

(defmacro emcp-defresource (name uri-or-template docstring &rest body)
  "Define NAME as an MCP resource or resource template.

URI-OR-TEMPLATE is a URI string, optionally containing {param}
placeholders per RFC 6570.  Parameter symbols are extracted
automatically and bound in BODY.  If there are no placeholders, NAME is
a static resource listed via resources/list; otherwise it is a resource
template listed via resources/templates/list.

The following keyword options may appear before BODY:

 :name MCP resource name (default NAME).
 :title MCP resource title.
 :description MCP resource description (default DOCSTRING).
 :mime-type MIME type of the resource content.
 :ttl-ms How many milliseconds clients may cache a read result
         (default 0).  Results are always cached privately.
 :async When non-nil, BODY sends its result or error via the locally
        bound functions `send-result' and `send-error'.  When nil, BODY
        returns a resource result directly.

When the client reads the resource, execute BODY to produce a result.
In addition to the template parameters, the following symbols are bound
in BODY:

 `server': The MCP server.
 `context': The `emcp-context' of the request.
 `uri': The full request URI.

If :async is nil or not provided, BODY just returns a resources/read
result alist as described in the MCP specification.  If :async is
non-nil, the following functions are bound and BODY has to call exactly
one of them once to send a result or error:

 `send-result' (RESULT): Send the resource RESULT to the client.
 `send-error' (CODE MESSAGE &optional DATA): Signal an error to the
 client with error code CODE, MESSAGE and optional error DATA.

If the client cancels the request, e.g. by closing the connection,
`send-result' and `send-error' do nothing.  BODY can register a
function that stops work that is no longer needed, or check for
cancellation itself:

  (emcp-context-on-cancel context #\\='stop-work an-arg)
  (unless (emcp-context-cancelled context) ...)

RESULT is a resources/read result JSON document (an alist) as described
in the spec, see this URL
https://modelcontextprotocol.io/specification/2026-07-28/server/resources

To report that the requested resource does not exist, BODY signals
`emcp-resource-not-found'.  Any other error is reported to the client as
an internal error."
  (declare (indent 2) (debug (symbolp stringp stringp body)))
  (let (mcp-name mcp-title mcp-description mime-type (ttl-ms 0) async-p)
    (while (keywordp (car body))
      (pcase (pop body)
        (:name (setq mcp-name (pop body)))
        (:title (setq mcp-title (pop body)))
        (:description (setq mcp-description (pop body)))
        (:mime-type (setq mime-type (pop body)))
        (:ttl-ms (setq ttl-ms (pop body)))
        (:async (setq async-p (pop body)))))
    (unless mcp-name
      (setq mcp-name (symbol-name name)))
    (unless mcp-description
      (setq mcp-description docstring))
    (let* ((args (emcp-uri--extract-params uri-or-template))
           (params-var (gensym "params"))
           (arg-bindings
            (cl-loop for sym in args
                     collect `(,sym (alist-get ',sym ,params-var))))
           (send-result-var (gensym "send-result"))
           (send-error-var (gensym "send-error")))
      (let ((metadata
             `((,(if args 'uriTemplate 'uri) . ,uri-or-template)
               (name . ,mcp-name)
               ,@(when mcp-title `((title . ,mcp-title)))
               (description . ,mcp-description)
               ,@(when mime-type `((mimeType . ,mime-type))))))
        (if args
            ;; Resource template
            `(progn
               (put ',name 'emcp-resource-template
                    (list :name ,uri-or-template
                          :metadata ',metadata
                          :match (emcp-uri--compile-template
                                  ,uri-or-template)
                          :ttl-ms ,ttl-ms))
               (defun ,name (server context ,send-result-var ,send-error-var uri ,params-var)
                 ,docstring
                 (ignore server context uri)
                 ,(if async-p
                      `(cl-flet ((send-result (result)
                                   (funcall ,send-result-var result))
                                 (send-error (code message &optional data)
                                   (funcall ,send-error-var code message data)))
                         (let ,arg-bindings
                           ,@body))
                    `(let ,arg-bindings
                       (funcall ,send-result-var (progn ,@body))))))
          ;; Static resource
          `(progn
             (put ',name 'emcp-resource
                  (list :name ,uri-or-template :metadata ',metadata :ttl-ms ,ttl-ms))
             (defun ,name (server context ,send-result-var ,send-error-var uri)
               ,docstring
               (ignore server context uri)
               ,(if async-p
                    `(cl-flet ((send-result (result)
                                 (funcall ,send-result-var result))
                               (send-error (code message &optional data)
                                 (funcall ,send-error-var code message data)))
                       ,@body)
                  `(funcall ,send-result-var (progn ,@body))))))))))

(provide 'emcp-core)
;;; emcp-core.el ends here
