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

;; All code related to the HTTP transport for MCP.

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
                    :extra-headers '(MCP-Session-Id MCP-Protocol-Version))))
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

(defun emcp--http-on-request (server request send-response)
  "Process an HTTP REQUEST to SERVER."
  (emcp--log server nil (trace (emcp--prefix-lines "> " (emcp--log-pp request))))
  (cl-flet ((invalid-path (path)
              (emcp--log server nil
                (warning (format "Invalid request path %s" (emcp--log-escape path))))
              (funcall send-response '(:status Not-Found)))
            (forbidden-origin (origin)
              (emcp--log server nil
                (warning (format "Forbidden origin %s" (emcp--log-escape origin))))
              (funcall send-response
                       `( :status Forbidden
                          :headers ((Content-Type . "application/json"))
                          :body ,(json-serialize
                                  (emcp--jsonrpc-error nil emcp--jsonrpc-invalid-request
                                                       "Forbidden origin")))))
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
              (emcp--log server (emcp--context-from-session server session)
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
    (let* ((path (plist-get request :path))
           (method (plist-get request :method))
           (headers (plist-get request :headers))
           (origin (alist-get 'Origin headers))
           (body (plist-get request :body)))
      (cond
       ((not (equal path (process-get (emcp--server-transport server) :emcp-path)))
        (invalid-path path))
       ((not (emcp-http--origin-allowed-p origin))
        (forbidden-origin origin))
       (t
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
               ;; Open an SSE connection for the server to send notifications
               (cl-flet ((body (send-chunk)
                           (cl-flet
                               ((send-notification (notification)
                                  (condition-case nil
                                      (if notification
                                          (let ((event (encode-coding-string
                                                        (format "data: %s\n\n" (json-serialize notification))
                                                        'utf-8)))
                                            (funcall send-chunk event :keep-open t))
                                        (funcall send-chunk ""))
                                    ;; Drop notifications on a closed
                                    ;; connection
                                    (http-server-client-disconnected
                                     (emcp--server-on-client-channel-closed server session)))))
                             (emcp--server-on-client-channel
                              server session #'send-notification))))
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
                     ((or 'result 'error)
                      ;; EMCP sends no requests, so we do not expect
                      ;; to receive any responses
                      (emcp--session-touch session)
                      (emcp--log server (emcp--context-from-session server session)
                        (warning (format "Rejecting unsolicited JSON-RPC response (%s)"
                                         (gethash "id" object)))
                        (debug (emcp--prefix-lines "> " (emcp--log-pp (emcp--to-alist object)))))
                      (let ((response
                             (emcp--jsonrpc-error nil emcp--jsonrpc-invalid-request
                                                  "Server sends no requests")))
                        (funcall send-response
                                 `( :status Bad-Request
                                    :headers ((Content-Type . "application/json"))
                                    :body ,(json-serialize response)))))
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
               (emcp--log server (emcp--context-from-session server session)
                 (info "Terminating session"))
               (emcp--server-delete-session server session)
               (funcall send-response '(:status No-Content))))))
          (_ (unexpected-method method))))))))

(provide 'emcp-http)
;;; emcp-http.el ends here
