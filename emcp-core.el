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

;; This file contains logging, the protocol-independent MCP server struct, the
;; capability-defining macros and similarly foundational facilities.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'seq)
(require 'url-parse)
(require 'url-util)

(require 'emcp-uri)

(define-error 'emcp-error "emcp")

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
  "Instructions to send to clients during initialization.

Should describe the overall purpose of the server."
  :group 'emcp
  :type 'string)

(defconst emcp-supported-versions '("2025-11-25")
  "Versions of the MCP spec supported by EMCP.

This is what is actually supported.  If your client refuses to connect,
you can try just adding your client's version and hoping for the best.")

;;; Logging

(eval-and-compile
  ;; Has to be available at compile time for emcp--log
  (defconst emcp-log-levels '(trace debug info warning error)
    "Possible log levels from most to least chatty."))

(defcustom emcp-log-level 'info
  "Current log level."
  :group 'emcp
  :type `(choice ,@(mapcar (lambda (l) `(const ,l)) emcp-log-levels)))

(defmacro emcp--log (server session &rest clauses)
  "Log to the log-buffer of SERVER.

SESSION is a session plist or nil.

CLAUSES are evaluated in turn, so one call can log at multiple log
levels.

Each clause is (LEVEL BODY...).  If the log-level of PROC is below LEVEL,
evaluate BODY for a message.  If the message is nil, nothing is logged."
  (declare (indent 2) (debug (form form &rest (symbolp body))))
  (let ((log-buffer (gensym "log-buffer"))
        (point-at-max (gensym "point-at-max"))
        (level-pos (gensym "level-pos"))
        (session-var (gensym "session"))
        (message (gensym "message")))
    `(when-let* ((,log-buffer (emcp--server-log-buffer ,server))
                 ((buffer-live-p ,log-buffer)))
       (with-current-buffer ,log-buffer
         (let* ((inhibit-read-only t)
                (,point-at-max (equal (point) (point-max)))
                (,level-pos (seq-position emcp-log-levels emcp-log-level))
                (,session-var ,session))
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
                                             ,(if session
                                                  `(substring (plist-get ,session-var :id) 0 6)
                                                (make-string 6 ?-)))))
                         (insert (emcp--prefix-lines prefix ,message) "\n")))))
                clauses))
           (when ,point-at-max
             (goto-char (point-max))))))))

(defvar emcp--log-escape-placeholder "×"
  "Replacement for non-ASCII characters in logs.")

(defconst emcp--log-escape-regexp
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

(defun emcp--jsonrpc-request (id method params)
  "Build a request for METHOD with ID and PARAMS."
  `((jsonrpc . "2.0")
    (method . ,method)
    ,@(when params `((params . ,params)))
    (id . ,id)))

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

CODE is an integer error code, MESSAGE is a short human-readable
description and DATA is arbitrary data related to the error."
  `((jsonrpc . "2.0")
    (id . ,(gethash "id" request))
    (error . ((code . ,code)
              (message . ,message)
              ,@(when data `((data . ,data)))))))

;;; MCP Server

(cl-defstruct (emcp--server (:constructor emcp--make-server)
                            (:copier nil))
  "An MCP server."
  (name "emcp" :type 'string :documentation "Server name")
  (log-buffer (with-current-buffer (generate-new-buffer (format "*%s*" name))
                (setq buffer-read-only t)
                (current-buffer)))
  transport
  (sessions (make-hash-table :test 'equal) :type 'hash-table
            :documentation "Active client sessions.")
  (next-request-id 1 :type 'int :documentation "Next server-sent request ID")
  (open-requests (make-hash-table :test 'equal) :type 'hash-table
                 :documentation "Request IDs and handlers waiting for responses")
  (prompts (make-hash-table :test 'equal) :type 'hash-table
           :documentation "Available prompt templates.")
  (resources (make-hash-table :test 'equal) :type 'hash-table
             :documentation "Available resources.")
  (resource-templates (make-hash-table :test 'equal) :type 'hash-table
                      :documentation "Available resource templates.")
  (tools (make-hash-table :test 'equal) :type 'hash-table
         :documentation "Available tools."))

(cl-defun emcp--server-build (&key name prompts resources tools)
  "Build a new MCP server from lists of capability symbols.

NAME is the server name.

PROMPTS, RESOURCES, and TOOLS are lists of symbols defined via
`emcp-defprompt', `emcp-defresource' and `emcp-deftool'."
  (let ((server (emcp--make-server :name name)))
    (emcp--server-reload server
                         :prompts prompts
                         :resources resources
                         :tools tools)
    server))

(defun emcp--server-stop (server)
  "Stop SERVER and clean up its log buffer."
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

  (cl-loop for session being the hash-values of (emcp--server-sessions server) do
           (emcp--server-send-notification server session "notifications/prompts/list_changed")
           (emcp--server-send-notification server session "notifications/resources/list_changed")
           (emcp--server-send-notification server session "notifications/tools/list_changed")))

(defun emcp--server-capabilities (_server)
  "Describe MCP server capabilities of SERVER."
  `((prompts . ((listChanged . t)))
    (resources . ((listChanged . t)
                  (subscribe . :false)))
    (tools . ((listChanged . t)))))

(defun emcp--server-on-initialize (server request)
  "Initialize a new session on SERVER.

REQUEST is an MCP initialization request.

Returns (SESSION . RESPONSE) where SESSION is the newly created session
and RESPONSE is the JSON-RPC response."
  (let* ((params (gethash "params" request))
         (requested-version (gethash "protocolVersion" params))
         (protocol-version
          (if (seq-contains-p emcp-supported-versions requested-version)
              requested-version
            (car emcp-supported-versions)))
         (icon (when-let* ((emcp-dir (file-name-directory (locate-library "emcp")))
                           (path (expand-file-name "emacs.svg" emcp-dir))
                           ((file-readable-p path))
                           (data (with-temp-buffer
                                   (insert-file-contents path)
                                   (buffer-string))))
                 (concat "data:image/svg+xml;base64," (base64-encode-string data t))))
         (capabilities (emcp--server-capabilities server))
         (session (list :id (emcp--make-session-id)
                        :state 'initializing
                        :created (current-time)
                        :protocol-version protocol-version
                        :client-capabilities (gethash "capabilities" params)
                        :server-capabilities capabilities
                        :client-info (gethash "clientInfo" params)
                        :client-channel nil
                        :roots nil))
         (response (emcp--jsonrpc-result
                    request
                    `((protocolVersion . ,protocol-version)
                      (capabilities . ,capabilities)
                      (serverInfo . ((name . "EMCP")
                                     (title . "EMCP - An MCP server for Emacs")
                                     (version . "0.1.0")
                                     (description . "Lets your agent talk to Emacs")
                                     (websiteUrl . "https://codeberg.org/martenlienen/emcp")
                                     ,@(when icon
                                         `((icons . [((mimeType . "image/svg+xml")
                                                      (sizes . ["any"])
                                                      (src . ,icon))])))))
                      (instructions . ,emcp-instructions)))))
    (emcp--log server session
      (info (format "New session %s" (plist-get session :id)))
      (debug (emcp--prefix-lines "| " (emcp--log-pp session))))
    (emcp--server-add-session server session)
    (cons session response)))

(defun emcp--server-add-session (server session)
  "Add a new SESSION to SERVER."
  (puthash (plist-get session :id) session (emcp--server-sessions server)))

(defun emcp--server-delete-session (server session)
  "Delete SESSION from SERVER."
  (remhash (plist-get session :id) (emcp--server-sessions server)))

(defun emcp--server-get-session (server session-id)
  "Get session with id SESSION-ID from SERVER."
  (gethash session-id (emcp--server-sessions server)))

(defun emcp--make-session-id ()
  "Create a new session id.

This is a copy of `org-id-uuid'."
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
                          (random)
                          (current-time)
                          (user-uid)
                          (emacs-pid)
                          (user-full-name)
                          user-mail-address
                          (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring rnd 0 8)
            (substring rnd 8 12)
            (substring rnd 13 16)
            (format "%x"
                    (logior
                     #b10000000
                     (logand
                      #b10111111
                      (string-to-number
                       (substring rnd 16 18) 16))))
            (substring rnd 18 20)
            (substring rnd 20 32))))

(defun emcp--server-on-client-channel (server session send-request)
  "A channel from SERVER to client has been established for SESSION.

The server will call (funcall SEND-REQUEST REQUEST) to send JSON-RPC
requests and notifications or (funcall SEND-REQUEST nil) to close the
channel."
  (emcp--log server session
    (info (format "Client channel established (%s)" (plist-get session :id))))
  (when-let* ((channel (plist-get session :client-channel)))
    ;; Close the previous channel
    (funcall channel nil))
  (plist-put session :client-channel send-request)
  (emcp--server-maybe-fetch-roots server session))

(defun emcp--server-on-client-channel-closed (_server session)
  "The client channel of SESSION on SERVER has been closed."
  (plist-put session :client-channel nil))

(defun emcp--server-client-channel-p (_server session)
  "Return t if a client channel for SESSION has been established."
  (when (plist-get session :client-channel)
    t))

(cl-defun emcp--server-send-request (server session method &key params on-result on-error)
  "Send a JSON-RPC request to the SESSION client on SERVER.

METHOD is the method to request and PARAMS are optional parameters for the
request.

When the client sends a response, (ON-RESULT RESULT) will be called on
success.  If the response is an error (ON-ERROR CODE MESSAGE DATA) will
be called instead."
  (if-let* ((channel (plist-get session :client-channel)))
      (let ((id (prog1 (emcp--server-next-request-id server)
                  (cl-incf (emcp--server-next-request-id server))))
            (handlers (cons (if on-result on-result #'ignore)
                            (if on-error on-error #'ignore))))
        (emcp--log server session
          (info (format "< Request %s (%s)" method id))
          (debug (if params
                     (emcp--prefix-lines "> " (emcp--log-pp params))
                   "No parameters")))
        (puthash id handlers (emcp--server-open-requests server))
        (funcall channel (emcp--jsonrpc-request id method params)))
    (emcp--log server session
      (warning "Tried to send notification, but no open channel"))))

(cl-defun emcp--server-send-notification (server session method &key params)
  "Send a JSON-RPC notification to the SESSION client on SERVER.

METHOD is the method to request and PARAMS are optional parameters for the
request."
  (when-let* ((channel (plist-get session :client-channel)))
    (emcp--log server session
      (info (format "< Notification %s" method))
      (debug (if params
                 (emcp--prefix-lines "< " (emcp--log-pp params))
               "No parameters")))
    (funcall channel (emcp--jsonrpc-notification method params))))

(defun emcp--server-on-request (server session request send-response)
  "Handle the MCP JSON-RPC2.0 request REQUEST on SERVER in SESSION.

SEND-RESPONSE is called with the response alist."
  (emcp--log server session
    (info
     (let* ((method (gethash "method" request))
            (params (gethash "params" request))
            (detail (pcase method
                      ((or "tools/call" "prompts/get") (gethash "name" params))
                      ((or "resources/read" "resources/subscribe" "resources/unsubscribe")
                       (gethash "uri" params)))))
       (format "> Request %s%s (%s)"
               method
               (if detail (format " %s" detail) "")
               (gethash "id" request))))
    (debug (if-let* ((params (gethash "params" request)))
               (emcp--prefix-lines "> " (emcp--log-pp (emcp--to-alist params)))
             "No parameters")))
  (cl-flet ((log-and-send (response)
              (emcp--log server session
                (info (format "Response %s (%s)"
                              (if (alist-get 'error response) "error" "ok")
                              (alist-get 'id response)))
                (debug (emcp--prefix-lines "< " (emcp--log-pp
                                                 (or (alist-get 'result response)
                                                     (alist-get 'error response))))))
              (funcall send-response response)))
    (handler-bind
        ;; Last resort response in case an error interrupts request handling
        ((error
          (lambda (err)
            (emcp--log server session
              (error (format "Error during request handling:\n%s"
                             (emcp--prefix-lines "| " (error-message-string err)))))
            (ignore-errors
              (log-and-send (emcp--jsonrpc-error
                             request
                             emcp--jsonrpc-internal-error
                             "Internal server error"))))))
      (pcase (gethash "method" request)
        ("prompts/list"
         (emcp--server-request--prompts-list server session request #'log-and-send))
        ("prompts/get"
         (emcp--server-request--prompts-get server session request #'log-and-send))
        ("resources/list"
         (emcp--server-request--resources-list server session request #'log-and-send))
        ("resources/templates/list"
         (emcp--server-request--resources-templates-list server session request #'log-and-send))
        ("resources/read"
         (emcp--server-request--resources-read server session request #'log-and-send))
        ("tools/list"
         (emcp--server-request--tools-list server session request #'log-and-send))
        ("tools/call"
         (emcp--server-request--tools-call server session request #'log-and-send))
        (_ (log-and-send (emcp--jsonrpc-error
                          request
                          emcp--jsonrpc-method-not-found
                          "Method not found")))))))

(defun emcp--server-request--prompts-list (server _session request send-response)
  "List all prompt definitions on SERVER.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((prompts (emcp--server-prompts server))
         (descriptions (cl-loop for prompt being the hash-values of prompts
                                collect (plist-get (get prompt 'emcp-prompt) :metadata))))
    (funcall send-response
             (emcp--jsonrpc-result request `((prompts . ,(vconcat descriptions)))))))

(defun emcp--server-request--prompts-get (server session request send-response)
  "Execute a prompt on SERVER in SESSION.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((params (gethash "params" request))
         (name (gethash "name" params))
         (prompt (gethash name (emcp--server-prompts server)))
         (args (gethash "arguments" params)))
    (cl-flet ((send-result (result)
                (funcall send-response (emcp--jsonrpc-result request result)))
              (send-error (code message &optional data)
                (funcall send-response (emcp--jsonrpc-error request code message data))))
      (funcall prompt server session #'send-result #'send-error args))))

(defun emcp--server-request--resources-list (server _session request send-response)
  "List all static resources on SERVER.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((resources (emcp--server-resources server))
         (descriptions (cl-loop for sym being the hash-values of resources
                                collect (plist-get (get sym 'emcp-resource) :metadata))))
    (funcall send-response
             (emcp--jsonrpc-result request `((resources . ,(vconcat descriptions)))))))

(defun emcp--server-request--resources-templates-list (server _session request send-response)
  "List all resource templates on SERVER.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((templates (emcp--server-resource-templates server))
         (descriptions (cl-loop for sym being the hash-values of templates
                                collect (plist-get (get sym 'emcp-resource-template) :metadata))))
    (funcall send-response
             (emcp--jsonrpc-result
              request `((resourceTemplates . ,(vconcat descriptions)))))))

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

(defun emcp--server-request--resources-read (server session request send-response)
  "Read a resource on SERVER in SESSION.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((params (gethash "params" request))
         (uri (gethash "uri" params)))
    (cl-flet ((send-result (result)
                (funcall send-response (emcp--jsonrpc-result request result)))
              (send-error (code message &optional data)
                (funcall send-response (emcp--jsonrpc-error request code message data))))
      (condition-case err
          (cond
           ;; Static resource
           ((when-let* ((sym (gethash uri (emcp--server-resources server))))
              (funcall sym server session #'send-result #'send-error uri)))
           ;; Resource template
           ((when-let* ((match (emcp--server-find-resource-template server uri)))
              (pcase-let ((`(,sym . ,uri-params) match))
                (funcall sym server session #'send-result #'send-error uri uri-params))))
           (t
            (send-error -32002 (format "No resource found for URI: %s" uri))))
        (error
         (send-error -32002 (error-message-string err)))))))

(defun emcp--server-request--tools-list (server _session request send-response)
  "List all tool definitions on SERVER.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((tools (emcp--server-tools server))
         (descriptions (cl-loop for tool being the hash-values of tools
                                collect (plist-get (get tool 'emcp-tool) :metadata))))
    (funcall send-response
             (emcp--jsonrpc-result request `((tools . ,(vconcat descriptions)))))))

(defun emcp--server-request--tools-call (server session request send-response)
  "Execute a tool on SERVER in SESSION.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((params (gethash "params" request))
         (name (gethash "name" params))
         (tool (gethash name (emcp--server-tools server)))
         (args (gethash "arguments" params)))
    (cl-flet ((send-result (result)
                (funcall send-response (emcp--jsonrpc-result request result)))
              (send-error (code message &optional data)
                (funcall send-response (emcp--jsonrpc-error request code message data))))
      (funcall tool server session #'send-result #'send-error args))))

(defun emcp--server-on-result (server session response)
  "Handle a successful JSONRPC2.0 RESPONSE on SERVER in SESSION."
  (emcp--log server session
    (info (format "Result (%s)" (gethash "id" response)))
    (debug (let ((result (emcp--to-alist (gethash "result" response))))
             (emcp--prefix-lines "> " (emcp--log-pp result)))))
  (let ((id (gethash "id" response))
        (open-requests (emcp--server-open-requests server)))
    (if-let* ((handler (gethash id open-requests)))
        (pcase-let ((`(,on-result . ,_) handler))
          (funcall on-result (gethash "result" response))
          (remhash id open-requests))
      (emcp--log server session
        (warning (format "Unexpected response ID %s" id))))))

(defun emcp--server-on-error (server session response)
  "Handle a JSONRPC2.0 error RESPONSE on SERVER in SESSION."
  (let* ((id (gethash "id" response))
         (open-requests (emcp--server-open-requests server))
         (error (gethash "error" response))
         (code (gethash "code" error))
         (message (gethash "message" error))
         (data (gethash "data" error)))
    (emcp--log server session
      (info (format "Error %s (%s): %s" code id message))
      (debug (emcp--prefix-lines "> " (emcp--log-pp (emcp--to-alist data)))))
    (if-let* ((handler (gethash id open-requests)))
        (pcase-let ((`(,_ . ,on-error) handler))
          (funcall on-error code message data)
          (remhash id open-requests))
      (emcp--log server session
        (warning (format "Unexpected response ID %s" id))))))

(defun emcp--normalize-root (root)
  "Normalize a ROOT object from a \"roots/list\" response.

ROOT is a hash table with a \"uri\" key and an optional \"name\" key.
The returned plist has:
  :uri   The URI exactly as the client sent it.
  :path  The decoded local filesystem path for \"file://\" URIs.
  :name  The client-provided name, or nil if absent."
  (when (hash-table-p root)
    (let* ((uri (gethash "uri" root))
           (name (gethash "name" root))
           (parsed (url-generic-parse-url uri))
           (path (when (equal (url-type parsed) "file")
                   (url-unhex-string (url-filename parsed)))))
      (list :uri uri :path path :name name))))

(defun emcp--server-fetch-roots (server session)
  "Request filesystem roots for a SESSION and store them in its :roots property.

SERVER is the server for SESSION."
  (emcp--server-send-request
   server session "roots/list"
   :on-result (lambda (result)
                (plist-put session :roots
                           (when (hash-table-p result)
                             (mapcar #'emcp--normalize-root
                                     (gethash "roots" result)))))
   :on-error (lambda (code message _data)
               (emcp--log server session
                 (warning (format "roots/list failed: %s (%s)" message code))))))

(defun emcp--client-supports-roots-p (session)
  "Return non-nil if SESSION's client declared the \"roots\" capability."
  (let ((caps (plist-get session :client-capabilities)))
    (and (hash-table-p caps) (gethash "roots" caps))))

(defun emcp--server-maybe-fetch-roots (server session)
  "Fetch SESSION's roots from SERVER's client if the time is right.

Requires the client to declare the \"roots\" capability, the session to
have completed initialization, a client channel to be open, and no
roots to be cached yet.  This is the single policy gate shared by the
three events that may trigger a fetch: the \"initialized\" notification,
the establishment of a client channel, and the \"roots/list_changed\"
notification (which clears the cache first)."
  (when (and (emcp--client-supports-roots-p session)
             (eq (plist-get session :state) 'up)
             (emcp--server-client-channel-p server session)
             (not (plist-get session :roots)))
    (emcp--server-fetch-roots server session)))

(defun emcp--server-on-notification (server session request)
  "Handle the MCP JSON-RPC2.0 notification REQUEST on SERVER in SESSION."
  (emcp--log server session
    (info (format "> Notification %s" (gethash "method" request)))
    (debug (when-let* ((params (gethash "params" request)))
             (emcp--prefix-lines "> " (emcp--log-pp (emcp--to-alist params))))))
  (handler-bind
      ((error
        (lambda (err)
          (emcp--log server session
            (error (format "Error during notification handling:\n%s"
                           (emcp--prefix-lines "| " (error-message-string err))))))))
    (pcase (gethash "method" request)
      ("notifications/initialized"
       (when (eq (plist-get session :state) 'initializing)
         (plist-put session :state 'up)
         (emcp--log server session
           (debug "Initialization complete"))
         (emcp--server-maybe-fetch-roots server session)))
      ("notifications/roots/list_changed"
       ;; Clear the old roots since they are invalid now, which also signals a re-fetch
       (plist-put session :roots nil)
       (emcp--server-maybe-fetch-roots server session)))))

(defun emcp--session-label (session)
  "Format a short, human-readable label for SESSION."
  (let* ((info (plist-get session :client-info))
         (title (and (hash-table-p info)
                     (or (gethash "title" info)
                         (gethash "name" info))))
         (root (car (plist-get session :roots)))
         (root-name (or (plist-get root :name)
                        (when-let* ((path (plist-get root :path)))
                          (file-name-nondirectory
                           (directory-file-name path)))))
         (detail (or root-name
                     (let ((id (plist-get session :id)))
                       (and id (substring id 0 (min 8 (length id)))))
                     "?")))
    (if title
        (format "%s (%s)" title detail)
      (format "(%s)" detail))))

;;; Capability defining macros

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
 :async When non-nil, BODY handles responses manually via locally bound
        functions `send-result' and `send-error'.  When nil, BODY
        returns a tool result directly.

When the client calls the tool, execute BODY to produce a result.  In
addition to the declared ARGS, the following symbols are bound:

 `server': The MCP server.
 `session': Client's MCP session.

If :async is nil or not provided, BODY just returns a tool result as
defined in the MCP specification.  If :async is non-nil, the following
functions are bound and BODY has to call exactly one of them once to
send a result or error:

 `send-result' (RESULT): Send the tool RESULT to the client.
 `send-error' (CODE MESSAGE &optional DATA): Signal an error to the
 client with error code CODE, MESSAGE and optional error DATA.

RESULT is a tool call result JSON document (an alist) as described in the
spec, see this URL
https://modelcontextprotocol.io/specification/2025-11-25/server/tools"
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
         (defun ,name (server session ,send-result-var ,send-error-var ,args-var)
           ,docstring
           (ignore server session)
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
 :async When non-nil, BODY handles responses manually via locally bound
	      functions `send-result' and `send-error'.  When nil, BODY
	      returns a prompt directly.

When the client requests the prompt, execute BODY to produce a
prompt.  In addition to the declared ARGS, the following symbols are
bound:

 `server': The MCP server.
 `session': Client's MCP session.

If :async is nil or not provided, BODY just returns a prompt as defined
in the MCP specification.  If :async is non-nil, the following functions
are bound and BODY has to call exactly one of them once to send a prompt
or error:

 `send-result' (PROMPT): Send the generated PROMPT to the client.
 `send-error' (CODE MESSAGE &optional DATA): Signal an error to the
 client with error code CODE, MESSAGE and optional error DATA.

RESULT is a prompt/get result JSON document (an alist) as described in the
spec, see this URL
https://modelcontextprotocol.io/specification/2025-11-25/server/prompts"
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
              ,@(when mcp-title `((title ,mcp-title)))
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
           (defun ,name (server session ,send-result-var ,send-error-var ,args-var)
             ,docstring
             (ignore server session)
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
 :async When non-nil, BODY handles responses manually via locally bound
        functions `send-result' and `send-error'.  When nil, BODY
        returns a resource result directly.

When the client reads the resource, execute BODY to produce a result.
In addition to the template parameters, the following symbols are bound
in BODY:

 `server': The MCP server.
 `session': Client's MCP session.
 `uri': The full request URI.

If :async is nil or not provided, BODY just returns a resources/read
result alist as described in the MCP specification.  If :async is
non-nil, the following functions are bound and BODY has to call exactly
one of them once to send a result or error:

 `send-result' (RESULT): Send the resource RESULT to the client.
 `send-error' (CODE MESSAGE &optional DATA): Signal an error to the
 client with error code CODE, MESSAGE and optional error DATA.

RESULT is a resources/read result JSON document (an alist) as described
in the spec, see this URL
https://modelcontextprotocol.io/specification/2025-11-25/server/resources"
  (declare (indent 2) (debug (symbolp stringp stringp body)))
  (let (mcp-name mcp-title mcp-description mime-type async-p)
    (while (keywordp (car body))
      (pcase (pop body)
        (:name (setq mcp-name (pop body)))
        (:title (setq mcp-title (pop body)))
        (:description (setq mcp-description (pop body)))
        (:mime-type (setq mime-type (pop body)))
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
                                  ,uri-or-template)))
               (defun ,name (server session ,send-result-var ,send-error-var uri ,params-var)
                 ,docstring
                 (ignore server session uri)
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
                  '(:name ,uri-or-template :metadata ,metadata))
             (defun ,name (server session ,send-result-var ,send-error-var uri)
               ,docstring
               (ignore server session uri)
               ,(if async-p
                    `(cl-flet ((send-result (result)
                                 (funcall ,send-result-var result))
                               (send-error (code message &optional data)
                                 (funcall ,send-error-var code message data)))
                       ,@body)
                  `(funcall ,send-result-var (progn ,@body))))))))))

(provide 'emcp-core)
;;; emcp-core.el ends here
