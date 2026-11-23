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

;; Two test clients: a core client that drives `emcp--server-on-request' directly without
;; any transport, and a raw HTTP client that talks to a server's HTTP transport over TCP.
;; The raw client is adapted from the test helpers in http-server.el.

;;; Code:

(require 'cl-lib)
(require 'emcp)
(require 'ert)

(defmacro emcp-test-with-server (server form &rest body)
  "Bind SERVER to FORM, execute BODY, then stop the server."
  (declare (indent 2) (debug (symbolp sexp body)))
  ;; Log at trace level to exercise logging code during tests
  `(let* ((emcp-log-level 'trace)
          (,server ,form))
     (unwind-protect
         (progn ,@body)
       (emcp--server-stop ,server))))

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

(defun emcp-test-to-hash-table (object)
  "Convert a JSON OBJECT to hash-table presentation."
  (json-parse-string (json-serialize object)))

(defmacro emcp-test-wait-for (form &optional timeout)
  "Process events until FORM is non-nil, for at most TIMEOUT seconds (default 1)."
  `(with-timeout (,(or timeout 1) (error "Timed out waiting for %S" ',form))
     (while (not ,form)
       (accept-process-output nil 0.01))))

;;; Core client

(cl-defstruct (emcp-test-client (:constructor emcp-test-make-client)
                                (:copier nil))
  "An MCP client for SERVER for testing.

Drives SERVER directly without any transport."
  server session next-request-id)

(cl-defun emcp-test-client-init-session (server &key on-notification)
  "Initialize a new client session for SERVER.

Calls (ON-NOTIFICATION NOTIFICATION) when the server sends a notification."
  (pcase-let ((`(,session ,_response)
               (emcp--server-on-initialize
                server
                (emcp-test-to-hash-table
                 '((id . 1) (params . ((protocolVersion . "2025-11-25"))))))))
    (when on-notification
      (emcp--server-on-client-channel server session on-notification))
    (emcp--server-on-notification
     server session
     (emcp-test-to-hash-table '((method . "notifications/initialized"))))
    (emcp-test-make-client :server server
                           :session session
                           :next-request-id 2)))

(defun emcp-test-client-send-request (client request callback)
  "Send REQUEST through CLIENT, calling CALLBACK with the response.

Adds a request `id' field to REQUEST automatically, unless already set."
  (unless (alist-get 'id request)
    (setf (alist-get 'id request) (emcp-test-client-next-request-id client))
    (cl-incf (emcp-test-client-next-request-id client)))
  (emcp--server-on-request
   (emcp-test-client-server client)
   (emcp-test-client-session client)
   (emcp-test-to-hash-table request)
   callback))

(defmacro emcp-test-client-with-response (client response request &rest body)
  "Send REQUEST through CLIENT and bind RESPONSE in BODY.

BODY can optionally start with the following keyword arguments:
  :timeout  Seconds to wait for the response (default 1).
  :wait     Whether to wait for the response (default t)."
  (declare (indent 3))
  (let ((timeout 1)
        (wait t))
    (while (keywordp (car body))
      (pcase (pop body)
        (:timeout (setq timeout (pop body)))
        (:wait (setq wait (pop body)))))
    (let ((done (gensym "done")))
      `(let ((,done nil))
         (emcp-test-client-send-request
          ,client ,request
          (lambda (,response)
            ,@body
            (setq ,done t)))
         ,@(when wait
             `((with-timeout (,timeout (error "Request timed out after %s seconds" ,timeout))
                 (while (not ,done)
                   (accept-process-output nil 0.01)))))))))

(defun emcp-test-request (client method &optional params)
  "Send METHOD with PARAMS through CLIENT and return the response alist.

New tests should prefer this over `emcp-test-client-with-response', so
that later changes to the request envelope only need to touch this
function."
  (let (response)
    (emcp-test-client-send-request
     client
     `((method . ,method) ,@(when params `((params . ,params))))
     (lambda (r) (setq response r)))
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
         (let* ((,client (emcp-test-client-init-session ,server))
                (,request `((method . "tools/call")
                            (params . ((name . ,,name)
                                       (arguments . ,,arguments))))))
           (emcp-test-client-with-response ,client ,response ,request
             ,@body))))))

;;; Raw HTTP client

(defmacro emcp-test-with-http (conn server &rest body)
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
         (when ,conn (emcp-test-http-disconnect ,conn))
         (kill-buffer ,buffer)))))

(defun emcp-test-http--default-headers (_body)
  "Return the default request headers for a POST of the JSON-RPC message BODY.

BODY is the message as passed to `emcp-test-http-post'.  Protocol headers
derived from BODY belong here, so that all tests send them by default."
  `(("Content-Type" . "application/json")
    ("Accept" . "application/json, text/event-stream")
    ("MCP-Protocol-Version" . ,(car emcp-supported-versions))))

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

(cl-defun emcp-test-http-post (conn body &key headers omit-headers path (timeout 1))
  "POST BODY on CONN and return the response as (:status :headers :body).

BODY is a JSON-RPC message as an alist or a string that is sent as is.
HEADERS is an alist of header names and values that are added to or
replace the default headers.  OMIT-HEADERS is a list of header names to
leave out.  PATH defaults to the MCP path of the server.  Header names
are strings and compared case-insensitively.  Wait at most TIMEOUT
seconds for the response.

Response headers are an alist of strings.  For an event stream, return
right after the response head with :body nil and leave the events for
`emcp-test-http-read-event'.  Otherwise, :body is the decoded response
body."
  (let* ((server (process-get conn :emcp-test-server))
         (transport (emcp--server-transport server))
         (payload (encode-coding-string
                   (if (stringp body) body (json-serialize body))
                   'utf-8))
         (all-headers (emcp-test-http--default-headers body)))
    (pcase-dolist (`(,name . ,value) headers)
      (setq all-headers (cl-remove name all-headers :key #'car :test #'string-equal-ignore-case))
      (push (cons name value) all-headers))
    (setq all-headers (cl-remove-if (lambda (h) (member-ignore-case (car h) omit-headers))
                                    all-headers))
    (process-send-string
     conn
     (concat (format "POST %s HTTP/1.1\r\n" (or path (process-get transport :emcp-path)))
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

Must follow an `emcp-test-http-post' that returned an event stream.
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

(defun emcp-test-http-disconnect (conn)
  "Close the client side of CONN."
  (delete-process conn))

(provide 'emcp-test-helpers)
;;; emcp-test-helpers.el ends here
