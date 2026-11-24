;;; emcp-test-helpers.el --- Shared helpers for the EMCP test suite -*- lexical-binding: t -*-

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

;; Two test clients: a core client that drives `emcp--request-context'
;; and `emcp--server-on-request' directly without any transport, and a
;; raw HTTP client that talks to a server's HTTP transport over TCP.
;; The raw client is adapted from the test helpers in http-server.el.

;;; Code:

(require 'cl-lib)
(require 'emcp)
(require 'ert)

(defmacro emcp-test-with-server (server form &rest body)
  "Bind SERVER to FORM, execute BODY, then destroy the server."
  (declare (indent 2) (debug (symbolp sexp body)))
  ;; Log at trace level to exercise logging code during tests
  `(let* ((emcp-log-level 'trace)
          (,server ,form))
     (unwind-protect
         (progn ,@body)
       (emcp--server-destroy ,server))))

(defun emcp-test-to-hash-table (object)
  "Convert the alist OBJECT to its JSON hash-table representation."
  (json-parse-string (json-serialize object)))

(defmacro emcp-test-wait-for (form &optional timeout)
  "Process events until FORM is non-nil, for at most TIMEOUT seconds (default 1)."
  `(with-timeout (,(or timeout 1) (error "Timed out waiting for %S" ',form))
     (while (not ,form)
       (accept-process-output nil 0.01))))

;;; Test primitives

(emcp-deftool emcp-test--tool () "A test tool." :name "tool"
              '((content . [((type . "text") (text . "tool result"))])))

(emcp-defprompt emcp-test--prompt () "A test prompt." :name "prompt"
                '((messages . [((role . "user") (content . ((type . "text") (text . "hi"))))])))

(emcp-defresource emcp-test--resource "emcp://test/static" "A static test resource."
                  `((contents . [((uri . ,uri) (text . "static"))])))

(emcp-defresource emcp-test--template "emcp://test/template/{id}" "A test template."
                  :ttl-ms 500
                  `((contents . [((uri . ,uri) (text . ,id))])))

(defconst emcp-test-primitives
  '( :tools (emcp-test--tool)
     :prompts (emcp-test--prompt)
     :resources (emcp-test--resource emcp-test--template))
  "Keyword arguments for `emcp--server-build' with one primitive of each kind.")

;;; Core client

(cl-defstruct (emcp-test-client (:constructor emcp-test-make-client)
                                (:copier nil))
  "An MCP client for SERVER for testing.

Drives SERVER directly without any transport.  CLIENT-INFO is the
clientInfo alist sent with every request, if any."
  server client-info (next-request-id 1))

(cl-defun emcp-test-make-request-meta
    (&key (protocol-version (car emcp-supported-versions))
          client-capabilities client-info log-level omit)
  "Build a request _meta alist.

PROTOCOL-VERSION, CLIENT-CAPABILITIES (default {}), CLIENT-INFO and
LOG-LEVEL set the respective fields; nil CLIENT-INFO and LOG-LEVEL leave
them out.  OMIT is a list of keywords among :protocol-version,
:client-capabilities and :client-info whose fields are left out."
  (let ((fields `((:protocol-version io.modelcontextprotocol/protocolVersion
                                     ,protocol-version)
                  (:client-capabilities io.modelcontextprotocol/clientCapabilities
                                        ,(or client-capabilities (make-hash-table)))
                  ,@(when client-info
                      `((:client-info io.modelcontextprotocol/clientInfo ,client-info)))
                  ,@(when log-level
                      `((:log-level io.modelcontextprotocol/logLevel ,log-level))))))
    (cl-loop for (keyword key value) in fields
             unless (memq keyword omit)
             collect (cons key value))))

(defun emcp-test-make-request (method &optional params &rest meta-args)
  "Build a JSON-RPC request for METHOD with PARAMS and a valid _meta.

META-ARGS override or omit _meta fields, see
`emcp-test-make-request-meta'."
  (list (cons 'jsonrpc "2.0")
        (cons 'id 1)
        (cons 'method method)
        (cons 'params `((_meta . ,(apply #'emcp-test-make-request-meta meta-args)) ,@params))))

(defun emcp-test-make-listen-request (id notifications)
  "Build a subscriptions/listen request with ID for the filter NOTIFICATIONS.

NOTIFICATIONS is an alist such as ((toolsListChanged . t))."
  (let ((request (emcp-test-make-request "subscriptions/listen"
                                         `((notifications . ,notifications)))))
    (setf (alist-get 'id request) id)
    request))

(defun emcp-test-client-send-request (client request callback &rest meta-args)
  "Send REQUEST through CLIENT, calling CALLBACK with the response.

Adds a request `id' field to REQUEST automatically, unless already set,
and the request _meta built from META-ARGS, see `emcp-test-make-request-meta'.
The clientInfo defaults to the one of CLIENT."
  (unless (alist-get 'id request)
    (setf (alist-get 'id request) (emcp-test-client-next-request-id client))
    (cl-incf (emcp-test-client-next-request-id client)))
  (let* ((server (emcp-test-client-server client))
         (meta (apply #'emcp-test-make-request-meta
                      (append meta-args
                              (list :client-info (emcp-test-client-client-info client)))))
         (params `((_meta . ,meta) ,@(alist-get 'params request)))
         (request (emcp-test-to-hash-table
                   `((params . ,params) ,@(cl-remove 'params request :key #'car)))))
    (emcp--server-on-request server (emcp--request-context server request) request callback)))

(defmacro emcp-test-client-with-response (client response request &rest body)
  "Send REQUEST through CLIENT and bind RESPONSE in BODY.

Wait for the response before executing BODY."
  (declare (indent 3))
  `(let (,response)
     (emcp-test-client-send-request ,client ,request (lambda (r) (setq ,response r)))
     (emcp-test-wait-for ,response)
     ,@body))

(defun emcp-test-response-data (response)
  "Return the result of RESPONSE without the MCP metadata fields.

These are resultType, _meta and the cache hints."
  (cl-remove-if (lambda (field) (memq (car field) '(resultType _meta ttlMs cacheScope)))
                (alist-get 'result response)))

(defun emcp-test-client-request (client method &optional params &rest meta-args)
  "Send METHOD with PARAMS through CLIENT and return the response alist.

META-ARGS are keyword arguments that override or omit _meta fields,
see `emcp-test-make-request-meta'."
  (let (response)
    (apply #'emcp-test-client-send-request
           client
           `((method . ,method) ,@(when params `((params . ,params))))
           (lambda (r) (setq response r))
           meta-args)
    (emcp-test-wait-for response)
    response))

(defmacro emcp-test-with-tool-response (response tool-symbol arguments &rest body)
  "Call TOOL-SYMBOL with ARGUMENTS and bind RESPONSE in BODY.

TOOL-SYMBOL is a symbol defined via `emcp-deftool'.  ARGUMENTS is an
alist of tool arguments (without the tool name).  RESPONSE is bound to
the full JSON-RPC response."
  (declare (indent 3) (debug (symbolp form form body)))
  (let ((name (gensym "name"))
        (server (gensym "server"))
        (client (gensym "client"))
        (request (gensym "request")))
    `(let ((,name (plist-get (get ,tool-symbol 'emcp-tool) :name)))
       (emcp-test-with-server ,server (emcp--server-build :tools (list ,tool-symbol))
         (let* ((,client (emcp-test-make-client :server ,server))
                (,request `((method . "tools/call")
                            (params . ((name . ,,name)
                                       (arguments . ,,arguments))))))
           (emcp-test-client-with-response ,client ,response ,request
             ,@body))))))

;;; Raw HTTP client

(defmacro emcp-test-with-http-server (server form &rest body)
  "Bind SERVER to FORM, start its HTTP transport, execute BODY, then stop both.

The transport listens on a random port."
  (declare (indent 2) (debug (symbolp sexp body)))
  `(emcp-test-with-server ,server ,form
     (let ((emcp-http-port t))
       (emcp-http--start-transport ,server))
     (unwind-protect
         (progn ,@body)
       (emcp-http--stop-transport ,server))))

(defmacro emcp-test-with-http-connection (conn server &rest body)
  "Bind CONN to a raw TCP connection to the HTTP transport of SERVER.

Execute BODY, then close CONN.  SERVER needs a running transport, see
`emcp-test-with-http-server'.  http-server closes the connection after
some responses, e.g. ones without a body, so use one connection per
request."
  (declare (indent 2) (debug (symbolp form body)))
  (let ((srv (gensym "server"))
        (buffer (gensym "buffer")))
    `(let* ((,srv ,server)
            (,buffer (generate-new-buffer " *emcp-test-http*"))
            (,conn nil))
       (unwind-protect
           (progn
             (setq ,conn (make-network-process
                          :name "emcp-test-http"
                          :buffer ,buffer
                          :host "127.0.0.1"
                          :service (process-contact (emcp--server-transport ,srv) :service)
                          :coding 'binary
                          :sentinel #'ignore))
             (process-put ,conn :emcp-test-server ,srv)
             ,@body)
         (when ,conn (delete-process ,conn))
         (kill-buffer ,buffer)))))

(defun emcp-test-http-encode-header-value (value)
  "Encode VALUE for an Mcp-Name header, using the base64 sentinel if needed."
  (if (or (string-match-p "[^\x20-\x7e]" value)
          (string-match-p "\\`[ \t]\\|[ \t]\\'" value)
          (string-match-p "\\`=\\?base64\\?.*\\?=\\'" value))
      (format "=?base64?%s?=" (base64-encode-string (encode-coding-string value 'utf-8) t))
    value))

(defun emcp-test-http--default-headers (body)
  "Return the default request headers for the JSON-RPC message BODY.

BODY is the message as passed to `emcp-test-http-request'.  Protocol
headers are derived from BODY, so that tests send valid requests by
default."
  (let* ((message (and (consp body) body))
         (method (alist-get 'method message))
         (params (alist-get 'params message))
         (version (or (alist-get 'io.modelcontextprotocol/protocolVersion
                                 (alist-get '_meta params))
                      (car emcp-supported-versions)))
         (name (pcase method
                 ((or "tools/call" "prompts/get") (alist-get 'name params))
                 ("resources/read" (alist-get 'uri params)))))
    `(("Content-Type" . "application/json")
      ("Accept" . "application/json, text/event-stream")
      ("MCP-Protocol-Version" . ,version)
      ,@(when (stringp method) `(("Mcp-Method" . ,method)))
      ,@(when (stringp name) `(("Mcp-Name" . ,(emcp-test-http-encode-header-value name)))))))

(defun emcp-test-http--response-head ()
  "Parse the response head at the start of the current buffer.

Return (STATUS HEADERS BODY-START) or nil if the head is incomplete."
  (goto-char (point-min))
  (when-let* ((head-end (search-forward "\r\n\r\n" nil t)))
    (goto-char (point-min))
    (unless (looking-at "HTTP/1\\.1 \\([0-9]+\\)")
      (error "Malformed status line: %S" (buffer-substring (point-min) (line-end-position))))
    (let ((status (string-to-number (match-string 1)))
          headers)
      (forward-line)
      (while (re-search-forward "^\\([^:\r\n]+\\): \\(.*\\)\r$" head-end t)
        (push (cons (match-string 1) (match-string 2)) headers))
      (list status (nreverse headers) head-end))))

(defun emcp-test-http--take-chunk ()
  "Remove the first complete chunk from the current buffer and return its data.

Return nil if no complete chunk has been received yet.  The final empty
chunk returns the empty string."
  (goto-char (point-min))
  (when (looking-at "\\([0-9a-fA-F]+\\)[^\r\n]*\r\n")
    (let* ((size (string-to-number (match-string 1) 16))
           (start (match-end 0))
           ;; Skip the CRLF after the data, which ends the empty trailer for the last chunk
           (end (+ start size 2)))
      (when (<= end (point-max))
        (prog1 (buffer-substring start (- end 2))
          (delete-region (point-min) end))))))

(cl-defun emcp-test-http-request (conn method body &key headers omit-headers path (timeout 1))
  "Send a METHOD request with BODY on CONN and return the response.

METHOD is an HTTP method symbol such as `GET'.  BODY is a JSON-RPC
message as an alist, a string that is sent as is, or nil for an empty
body.  HEADERS is an alist of header names and values that are added to
or replace the default headers.  OMIT-HEADERS is a list of header names
to leave out.  PATH defaults to the MCP path of the server.  Header
names are strings and compared case-insensitively.  Wait at most TIMEOUT
seconds for the response.

The response is a plist (:status :headers :body).  Response headers are
an alist of strings.  For an event stream, return right after the
response head with :body nil and leave the events for
`emcp-test-http-read-event'.  Otherwise, :body is the decoded response
body."
  (let* ((server (process-get conn :emcp-test-server))
         (transport (emcp--server-transport server))
         (payload (encode-coding-string
                   (cond ((null body) "")
                         ((stringp body) body)
                         (t (json-serialize body)))
                   'utf-8))
         (all-headers (emcp-test-http--default-headers body)))
    (pcase-dolist (`(,name . ,value) headers)
      (setq all-headers (cl-remove name all-headers :key #'car :test #'string-equal-ignore-case))
      (push (cons name value) all-headers))
    (setq all-headers (cl-remove-if (lambda (h) (member-ignore-case (car h) omit-headers))
                                    all-headers))
    (process-send-string
     conn
     (concat (format "%s %s HTTP/1.1\r\n" method (or path (process-get transport :emcp-path)))
             (format "Host: 127.0.0.1:%s\r\n" (process-contact transport :service))
             (mapconcat (pcase-lambda (`(,name . ,value)) (format "%s: %s\r\n" name value))
                        all-headers "")
             (format "Content-Length: %d\r\n\r\n" (length payload))
             payload))
    (with-current-buffer (process-buffer conn)
      (with-timeout (timeout (error "Incomplete response: %S" (buffer-string)))
        (let (head)
          (while (not (setq head (emcp-test-http--response-head)))
            (accept-process-output conn 0.01))
          (pcase-let* ((`(,status ,response-headers ,body-start) head)
                       (header (lambda (name)
                                 (cdr (assoc-string name response-headers t))))
                       (content-length (funcall header "Content-Length"))
                       (chunked (equal (funcall header "Transfer-Encoding") "chunked")))
            (delete-region (point-min) body-start)
            (list :status status
                  :headers response-headers
                  :body (cond
                         ((string-prefix-p "text/event-stream"
                                           (or (funcall header "Content-Type") ""))
                          nil)
                         (chunked
                          (let (chunks chunk)
                            (while (not (equal chunk ""))
                              (if (setq chunk (emcp-test-http--take-chunk))
                                  (push chunk chunks)
                                (accept-process-output conn 0.01)))
                            (decode-coding-string (apply #'concat (nreverse chunks)) 'utf-8)))
                         (t
                          (let ((n (if content-length (string-to-number content-length) 0)))
                            (while (< (buffer-size) n)
                              (accept-process-output conn 0.01))
                            (prog1 (decode-coding-string
                                    (buffer-substring (point-min) (+ (point-min) n))
                                    'utf-8)
                              (delete-region (point-min) (+ (point-min) n)))))))))))))

(defun emcp-test-http--parse-event (event)
  "Parse the SSE EVENT string.

Return :comment if EVENT has no data lines, otherwise its parsed JSON data."
  (let (data)
    (dolist (line (split-string event "\n"))
      (when (string-match "\\`data: ?\\(.*\\)" line)
        (push (match-string 1 line) data)))
    (if data
        (json-parse-string (decode-coding-string (string-join (nreverse data) "\n") 'utf-8)
                           :object-type 'alist :null-object nil)
      :comment)))

(cl-defun emcp-test-http-read-event (conn &key (timeout 1))
  "Read the next SSE event from the response on CONN.

Must follow an `emcp-test-http-request' that returned an event stream.
Return :comment for an event that consists only of comment lines, the
parsed JSON data of a data event, or nil if the stream has ended.  Wait
at most TIMEOUT seconds for the event."
  (with-current-buffer (process-buffer conn)
    (with-timeout (timeout (error "No complete event: %S" (buffer-string)))
      (let ((pending (or (process-get conn :emcp-test-sse) ""))
            event)
        (while (and (not event)
                    (or (string-search "\n\n" pending)
                        (not (process-get conn :emcp-test-sse-ended))))
          (if-let* ((end (string-search "\n\n" pending)))
              (setq event (substring pending 0 end)
                    pending (substring pending (+ end 2)))
            (if-let* ((chunk (emcp-test-http--take-chunk)))
                (if (equal chunk "")
                    (process-put conn :emcp-test-sse-ended t)
                  (setq pending (concat pending chunk)))
              (accept-process-output conn 0.01))))
        (process-put conn :emcp-test-sse pending)
        (and event (emcp-test-http--parse-event event))))))

(provide 'emcp-test-helpers)
;;; emcp-test-helpers.el ends here
