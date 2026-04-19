;;; emcp.el --- An MCP server for Emacs -*- lexical-binding: t -*-

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

;; EMCP lets you connect your LLM agent directly to Emacs through an MCP server.

;;; Code:

(require 'cl-lib)
(require 'http-server)
(require 'rx)
(require 'seq)

(define-error 'emcp-error "emcp")

(defgroup emcp ()
  "An Emacs MCP server."
  :group 'environment)

(defcustom emcp-http-host "127.0.0.1"
  "Host address to bind to for HTTP connections."
  :group 'emcp
  :type 'string)

(defcustom emcp-http-port t
  "Port for the server to listen on for HTTP connections.

t means arbitrary choice at runtime."
  :group 'emcp
  :type '(choice integer (const t)))

(defcustom emcp-http-path "/mcp"
  "Path where the server is available via HTTP."
  :group 'emcp
  :type 'string)

(defcustom emcp-instructions "Interact with a running Emacs instance.

Use this when you are
- answering questions about Emacs,
- debugging Emacs,
- programming or planning Emacs Lisp,
- or controlling an Emacs instance."
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
         (let* ((,point-at-max (equal (point) (point-max)))
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
  (log-buffer (generate-new-buffer (format "*%s*" name)))
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

(defun emcp--server-stop (server)
  "Stop SERVER, shutdown its transport and clean up the log buffer."
  (when-let* ((transport (emcp--server-transport server)))
    (http-server-stop transport))
  (kill-buffer (emcp--server-log-buffer server)))

(cl-defun emcp--server-start-http (server &key (log-level 'info))
  "Start an HTTP transport for SERVER.

LOG-LEVEL is the HTTP log level."
  (let ((transport (http-server-start
                    :name (concat (emcp--server-name server) ":http")
                    :host emcp-http-host
                    :port emcp-http-port
                    :on-request (lambda (request send-response)
                                  (funcall #'emcp--http-on-request server request send-response))
                    :log-level log-level
                    :extra-headers '(MCP-Session-Id MCP-Protocol-Version))))
    (setf (emcp--server-transport server) transport)
    (process-put transport :emcp-path emcp-http-path)
    (emcp--log server nil (info "HTTP transport started"))))

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
         (session `( :id ,(emcp--make-session-id)
                     :state initializing
                     :protocol-version ,protocol-version
                     :client-capabilities ,(gethash "capabilities" params)
                     :server-capabilities ,capabilities
                     :client-info ,(gethash "clientInfo" params)
                     :client-channel nil))
         (response (emcp--jsonrpc-result
                    request
                    `((protocolVersion . ,protocol-version)
                      (capabilities . ,capabilities)
                      (serverInfo . ((name . "EMCP")
                                     (title . "EMCP - An MCP server for Emacs")
                                     (version . "0.1.0")
                                     (description . "An MCP server for Emacs")
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
  (plist-put session :client-channel send-request))

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
         (emcp--server-request-prompts/list server session request #'log-and-send))
        ("prompts/get"
         (emcp--server-request-prompts/get server session request #'log-and-send))
        ("resources/list"
         (emcp--server-request-resources/list server session request #'log-and-send))
        ("resources/templates/list"
         (emcp--server-request-resources/templates/list server session request #'log-and-send))
        ("resources/read"
         (emcp--server-request-resources/read server session request #'log-and-send))
        ("tools/list"
         (emcp--server-request-tools/list server session request #'log-and-send))
        ("tools/call"
         (emcp--server-request-tools/call server session request #'log-and-send))
        (_ (log-and-send (emcp--jsonrpc-error
                          request
                          emcp--jsonrpc-method-not-found
                          "Method not found")))))))

(defun emcp--server-request-prompts/list (server _session request send-response)
  "List all prompt definitions on SERVER.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((prompts (emcp--server-prompts server))
         (descriptions (cl-loop for prompt being the hash-values of prompts
                                collect (plist-get (get prompt 'emcp-prompt) :metadata))))
    (funcall send-response
             (emcp--jsonrpc-result request `((prompts . ,(vconcat descriptions)))))))

(defun emcp--server-request-prompts/get (server session request send-response)
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

(defun emcp--server-request-resources/list (server _session request send-response)
  "List all static resources on SERVER.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((resources (emcp--server-resources server))
         (descriptions (cl-loop for sym being the hash-values of resources
                                collect (plist-get (get sym 'emcp-resource) :metadata))))
    (funcall send-response
             (emcp--jsonrpc-result request `((resources . ,(vconcat descriptions)))))))

(defun emcp--server-request-resources/templates/list (server _session request send-response)
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
                     (params (emcp-resources--match-uri uri (plist-get template :match))))
           (setq result (cons sym params)))))
     (emcp--server-resource-templates server))
    result))

(defun emcp--server-request-resources/read (server session request send-response)
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

(defun emcp--server-request-tools/list (server _session request send-response)
  "List all tool definitions on SERVER.

SEND-RESPONSE is called with the response to REQUEST."
  (let* ((tools (emcp--server-tools server))
         (descriptions (cl-loop for tool being the hash-values of tools
                                collect (plist-get (get tool 'emcp-tool) :metadata))))
    (funcall send-response
             (emcp--jsonrpc-result request `((tools . ,(vconcat descriptions)))))))

(defun emcp--server-request-tools/call (server session request send-response)
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
    (when (and (eq (plist-get session :state) 'initializing)
               (equal (gethash "method" request) "notifications/initialized"))
      (plist-put session :state 'up)
      (emcp--log server session
        (debug "Initialization complete")))))

;;; HTTP transport

(defun emcp--http-on-request (server request send-response)
  "Process an HTTP REQUEST to SERVER."
  (emcp--log server nil (trace (emcp--prefix-lines "> " (emcp--log-pp request))))
  (cl-flet ((invalid-path (path)
              (emcp--log server nil
                (warning (format "Invalid request path %s" (emcp--log-escape path))))
              (funcall send-response '(:status Not-Found)))
            (unexpected-method (method)
              (emcp--log server nil
                (debug (format "Unexpected request method %s" method)))
              (funcall send-response '(:status Method-Not-Allowed)))
            (no-session-id ()
              (emcp--log server nil
                (warning "Missing MCP-Session-Id on non-initialize request"))
              (funcall send-response '(:status Bad-Request)))
            (invalid-session-id (session-id)
              (emcp--log server nil
                (warning (format "Invalid session ID %s" session-id)))
              (funcall send-response '(:status Not-Found)))
            (invalid-request (session request)
              (emcp--log server session
                (warning "Invalid JSON-RPC request")
                (debug (emcp--prefix-lines "> " (emcp--log-pp (emcp--to-alist request)))))
              (funcall send-response '(:status Bad-Request)))
            (protocol-versions-match-p (session headers)
              (let ((header-version (alist-get 'MCP-Protocol-Version headers)))
                (or (not header-version) (equal header-version (plist-get session :protocol-version)))))
            (protocol-version-mismatch (session headers)
              (let ((header-version (alist-get 'MCP-Protocol-Version headers))
                    (session-version (plist-get session :protocol-version)))
                (emcp--log server nil
                  (warning (format "Request protocol version %s, but %s was negotiated"
                                   header-version session-version))))
              (funcall send-response '(:status Bad-Request))))
    (let ((path (plist-get request :path))
          (method (plist-get request :method))
          (headers (plist-get request :headers))
          (body (plist-get request :body)))
      (if (not (equal path (process-get (emcp--server-transport server) :emcp-path)))
          (invalid-path path)
        (pcase method
          ('GET
           (let* ((session-id (alist-get 'MCP-Session-Id headers))
                  (session (emcp--server-get-session server session-id)))
             (cond
              ((not session-id) (no-session-id))
              ((not session) (invalid-session-id session-id))
              ((not (protocol-versions-match-p session headers))
               (protocol-version-mismatch session headers))
              (t
               ;; Open an SSE connection for the server to send requests and notifications
               (cl-flet ((body (send-chunk)
                           (cl-flet
                               ((send-request (request)
                                  (condition-case err
                                      (if request
                                          (let ((event (encode-coding-string
                                                        (format "data: %s\n\n" (json-serialize request))
                                                        'utf-8)))
                                            (funcall send-chunk event :keep-open t))
                                        (funcall send-chunk ""))
                                    (http-server-client-disconnected
                                     (emcp--server-on-client-channel-closed server session)
                                     ;; For notifications, we just drop them
                                     (when (eq (emcp--jsonrpc-type request) 'request)
                                       (signal 'emcp-error `("Cannot send request on closed connection:"
                                                             ,(error-message-string err))))))))
                             (emcp--server-on-client-channel
                              server session #'send-request))))
                 (funcall send-response
                          `( :status OK
                             :headers ((Content-Type . "text/event-stream")
                                       (Cache-Control . "no-cache"))
                             :body ,#'body)))))))
          ('POST
           (condition-case _err
               (let* ((session-id (alist-get 'MCP-Session-Id headers))
                      (session (emcp--server-get-session server session-id))
                      (object (and body (json-parse-string body))))
                 (cond
                  ((not session-id)
                   (if (and (eq (emcp--jsonrpc-type object) 'request)
                            (equal (gethash "method" object) "initialize"))
                       (pcase-let ((`(,session . ,response) (emcp--server-on-initialize server object)))
                         (funcall send-response
                                  `( :status Created
                                     :headers ((MCP-Session-Id . ,(plist-get session :id))
                                               (Content-Type . "application/json"))
                                     :body ,(json-serialize response))))
                     (no-session-id)))
                  ((not session) (invalid-session-id session-id))
                  ((not (protocol-versions-match-p session headers))
                   (protocol-version-mismatch session headers))
                  (t
                   (pcase (emcp--jsonrpc-type object)
                     ('request
                      (emcp--server-on-request
                       server session object
                       (lambda (response)
                         (funcall send-response
                                  `( :status OK
                                     :headers ((Content-Type . "application/json"))
                                     :body ,(json-serialize response))))))
                     ('result
                      (emcp--server-on-result server session object))
                     ('error
                      (emcp--server-on-error server session object))
                     ('notification
                      (emcp--server-on-notification server session object)
                      (funcall send-response '(:status Accepted)))
                     (_
                      (invalid-request session object))))))
             (json-parse-error
              (emcp--log server nil
                (warning (format "Failed to parse JSON-RPC request:\n%s"
                                 (emcp--prefix-lines
                                  "> " (emcp--log-fill (emcp--log-escape body))))))
              (funcall send-response '(:status Bad-Request)))))
          ('DELETE
           (let* ((session-id (alist-get 'MCP-Session-Id headers))
                  (session (emcp--server-get-session server session-id)))
             (cond
              ((not session-id) (no-session-id))
              ((not session) (invalid-session-id session-id))
              ((not (protocol-versions-match-p session headers))
               (protocol-version-mismatch session headers))
              (t
               (emcp--log server session
                 (info "Terminating session"))
               (emcp--server-delete-session server session)
               (funcall send-response '(:status No-Content))))))
          (_ (unexpected-method method)))))))

;;; Public interface

(require 'emcp-prompts)
(require 'emcp-resources)
(require 'emcp-tools)

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
                 :tools (emcp-tools-screenshot))))
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
      (emcp--server-start-http server)
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
        (emcp--server-stop running)
        (setq emcp--servers (assq-delete-all profile emcp--servers))
        (let* ((resolved (emcp--resolve-profile (alist-get profile emcp-profiles)))
               (server (emcp--server-build :name (format "emcp-%s" profile)
                                           :prompts (plist-get resolved :prompts)
                                           :resources (plist-get resolved :resources)
                                           :tools (plist-get resolved :tools))))
          (emcp--server-start-http server)
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
