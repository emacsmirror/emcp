;;; emcp-http.el --- The HTTP MCP transport -*- lexical-binding: t -*-

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

;; The stateless Streamable HTTP transport of MCP 2026-07-28 [1].
;;
;; [1] https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/streamable-http

;;; Code:

(require 'http-server)
(require 'url-parse)

(require 'emcp-core)

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

(cl-defun emcp-http--start-transport (server &key (log-level 'info))
  "Start an HTTP transport for SERVER.

LOG-LEVEL is the HTTP log level."
  (let ((transport (http-server-start
                    :name (concat (emcp--server-name server) ":http")
                    :host emcp-http-host
                    :port emcp-http-port
                    :on-request (lambda (request send-response)
                                  (funcall #'emcp--http-on-request server request send-response))
                    :log-level log-level
                    :extra-headers '(MCP-Protocol-Version Mcp-Method Mcp-Name))))
    (setf (emcp--server-transport server) transport)
    (process-put transport :emcp-path emcp-http-path)
    (emcp--log server nil (info "HTTP transport started"))))

(defun emcp-http--stop-transport (server)
  "Shut down the HTTP transport of SERVER."
  (when-let* ((transport (emcp--server-transport server)))
    (http-server-stop transport)))

(defun emcp-http--origin-allowed-p (origin)
  "Return non-nil if a request with Origin header ORIGIN may access the server.

Only requests without an Origin header or with a local http(s) origin
are allowed, as the MCP specification requires to prevent DNS rebinding
attacks."
  (or (not origin)
      (let ((url (url-generic-parse-url origin)))
        (and (member (url-type url) '("http" "https"))
             ;; The parser keeps the brackets of IPv6 hosts and downcases hosts
             (member (url-host url) '("localhost" "127.0.0.1" "[::1]"))))))

(defun emcp-http--decode-header-value (value)
  "Decode the header VALUE, which may use the base64 sentinel encoding.

Return the decoded string or nil if VALUE is not valid base64-encoded
UTF-8."
  (if (let ((case-fold-search nil))
        ;; The spec requires the markers in lowercase
        (string-match "\\`=\\?base64\\?\\(.*\\)\\?=\\'" value))
      (when-let* ((bytes (ignore-errors (base64-decode-string (match-string 1 value))))
                  (decoded (decode-coding-string bytes 'utf-8-unix))
                  ;; Invalid UTF-8 decodes to raw bytes of the eight-bit
                  ;; charset
                  ((not (memq 'eight-bit (find-charset-string decoded)))))
        decoded)
    value))

(defun emcp-http--mirrored-header-mismatch (headers request)
  "Compare the HTTP HEADERS that mirror fields of the JSON-RPC REQUEST.

The spec requires MCP-Protocol-Version, Mcp-Method and, for some
methods, Mcp-Name to repeat body fields for HTTP intermediaries.  Return
nil if they are present and match the body or a message describing the
mismatch."
  (cond*
   ((bind* (header-version (alist-get 'MCP-Protocol-Version headers))
           (header-method (alist-get 'Mcp-Method headers))
           (header-name (alist-get 'Mcp-Name headers))
           (method (gethash "method" request))
           (params (gethash "params" request))
           (meta (and (hash-table-p params) (gethash "_meta" params)))
           (version (and (hash-table-p meta)
                         (gethash "io.modelcontextprotocol/protocolVersion" meta)))
           ;; Only these methods mirror a name into Mcp-Name
           (name-field (pcase method
                         ((or "tools/call" "prompts/get") "name")
                         ("resources/read" "uri")))))
   ((not header-version)
    "Missing MCP-Protocol-Version header")
   ;; `emcp--request-context' reports a missing body version as invalid params
   ((and (stringp version) (not (equal header-version version)))
    (format "MCP-Protocol-Version header value '%s' does not match body value '%s'"
            header-version version))
   ((not header-method)
    "Missing Mcp-Method header")
   ((not (equal header-method method))
    (format "Mcp-Method header value '%s' does not match body value '%s'"
            header-method method))
   ((not name-field)
    nil)
   ((not header-name)
    "Missing Mcp-Name header")
   ((bind* (decoded-name (emcp-http--decode-header-value header-name))
           (name (and (hash-table-p params) (gethash name-field params)))))
   ((not decoded-name)
    (format "Mcp-Name header value '%s' is not valid base64-encoded UTF-8" header-name))
   ((not (equal decoded-name name))
    (format "Mcp-Name header value '%s' does not match body value '%s'"
            decoded-name name))))

(defun emcp--http-on-request (server http-request send-response)
  "Process the HTTP-REQUEST to SERVER."
  (emcp--log server nil (trace (emcp--prefix-lines "> " (emcp--log-pp http-request))))
  ;; First, define all possible outcomes for a request including
  ;; logging and response, so that the actual flow logic below stays
  ;; compact.
  (cl-flet* ((send-json (status json)
               (funcall send-response
                        `( :status ,status
                           :headers ((Content-Type . "application/json"))
                           :body ,(json-serialize json))))
             (log-message (rpc-message)
               (emcp--prefix-lines "> " (emcp--log-pp (emcp--to-alist rpc-message))))
             (invalid-path (path)
               (emcp--log server nil
                 (warning (format "Invalid request path %s" (emcp--log-escape path))))
               (funcall send-response '(:status Not-Found)))
             (forbidden-origin (origin)
               (emcp--log server nil
                 (warning (format "Forbidden origin %s" (emcp--log-escape origin))))
               (send-json 'Forbidden (emcp--jsonrpc-error nil emcp--jsonrpc-invalid-request
                                                          "Forbidden origin")))
             (method-not-allowed (method)
               (emcp--log server nil
                 (debug (format "Unexpected request method %s" method)))
               (funcall send-response '( :status Method-Not-Allowed
                                         :headers ((Allow . "POST")))))
             (invalid-json (body)
               (emcp--log server nil
                 (warning (format "Failed to parse JSON-RPC message:\n%s"
                                  (emcp--prefix-lines
                                   "> " (emcp--log-fill (emcp--log-escape body))))))
               (send-json 'Bad-Request (emcp--jsonrpc-error nil emcp--jsonrpc-parse-error
                                                            "Parse error")))
             (response-from-client (response)
               (emcp--log server nil
                 (warning (format "Rejecting JSON-RPC response (%s)" (gethash "id" response)))
                 (debug (log-message response)))
               (send-json 'Bad-Request (emcp--jsonrpc-error nil emcp--jsonrpc-invalid-request
                                                            "Server sends no requests")))
             (notification (notification)
               (emcp--log server nil
                 (info (format "> Notification %s" (gethash "method" notification)))
                 (debug (log-message notification)))
               (funcall send-response '(:status Accepted)))
             (invalid-message (rpc-message)
               (emcp--log server nil
                 (warning "Invalid JSON-RPC message")
                 (debug (log-message rpc-message)))
               (send-json 'Bad-Request (emcp--jsonrpc-error nil emcp--jsonrpc-invalid-request
                                                            "Invalid request")))
             (legacy-initialize (request)
               (emcp--log server nil
                 (warning "Rejecting initialize from a legacy client")
                 (debug (log-message request)))
               (let* ((params (gethash "params" request))
                      (requested (and (hash-table-p params)
                                      (gethash "protocolVersion" params))))
                 (send-json 'Bad-Request
                            (emcp--jsonrpc-error
                             request emcp--mcp-unsupported-protocol-version
                             (format (concat "Unsupported protocol version: EMCP only "
                                             "supports MCP %s, which has no initialize "
                                             "request")
                                     (string-join emcp-supported-versions ", "))
                             `((supported . ,(vconcat emcp-supported-versions))
                               (requested . ,(if (stringp requested) requested :null)))))))
             (mismatching-headers (request mismatch)
               (emcp--log server nil
                 (warning (format "Header mismatch: %s" (emcp--log-escape mismatch)))
                 (debug (log-message request)))
               (send-json 'Bad-Request (emcp--jsonrpc-error
                                        request emcp--mcp-header-mismatch
                                        (format "Header mismatch: %s" mismatch))))
             (invalid-request (request code error-message data)
               (emcp--log server nil
                 (warning (format "Rejecting request (%s): %s"
                                  (gethash "id" request) (emcp--log-escape error-message)))
                 (debug (log-message request)))
               (send-json 'Bad-Request
                          (emcp--jsonrpc-error request code error-message data)))
             (send-rpc-response (response)
               (let ((status (if (eql (alist-get 'code (alist-get 'error response))
                                      emcp--jsonrpc-method-not-found)
                                 'Not-Found
                               'OK)))
                 (send-json status response))))
    (cond*
     ((bind* (path (plist-get http-request :path))
             (method (plist-get http-request :method))
             (headers (plist-get http-request :headers))
             (origin (alist-get 'Origin headers))
             (body (or (plist-get http-request :body) ""))))
     ;; Block all requests to unexpected paths
     ((not (equal path (process-get (emcp--server-transport server) :emcp-path)))
      (invalid-path path))
     ;; Block requests with unexpected Origin headers to prevent DNS
     ;; rebinding attacks
     ((not (emcp-http--origin-allowed-p origin))
      (forbidden-origin origin))
     ;; The 2026-07-28 MCP spec only allows POST requests
     ((not (eq method 'POST))
      (method-not-allowed method))
     ;; Reject invalid JSON bodies
     ((bind* (rpc-message (condition-case nil
                              (json-parse-string body)
                            (json-error nil)))))
     ((not rpc-message)
      (invalid-json body))
     ;; Reject all JSON-RPC types except requests
     ((bind* (type (emcp--jsonrpc-type rpc-message))))
     ;; EMCP sends no requests, so clients must not send responses
     ((memq type '(result error))
      (response-from-client rpc-message))
     ;; The spec defines no client notifications for HTTP
     ((eq type 'notification)
      (notification rpc-message))
     ((not (eq type 'request))
      (invalid-message rpc-message))
     ;; Legacy clients start with initialize and have no other way to
     ;; learn which versions EMCP supports, see "Backward
     ;; Compatibility" in the versioning section of the spec
     ((equal (gethash "method" rpc-message) "initialize")
      (legacy-initialize rpc-message))
     ;; Ensure that the info in the HTTP headers matches the actual
     ;; request
     ((bind-and* (mismatch (emcp-http--mirrored-header-mismatch headers rpc-message)))
      (mismatching-headers rpc-message mismatch))
     (t
      (condition-case outcome
          (emcp--request-context server rpc-message)
        (:success
         (emcp--server-on-request server outcome rpc-message #'send-rpc-response))
        (emcp-invalid-request
         (apply #'invalid-request rpc-message (cdr outcome))))))))

(provide 'emcp-http)
;;; emcp-http.el ends here
