;;; emcp-http-tests.el --- Tests for the HTTP transport -*- lexical-binding: t -*-

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

;; Tests for the HTTP transport in emcp-http.el through a real TCP connection.

;;; Code:

(require 'emcp-http)
(require 'emcp-test-helpers)
(require 'ert)

(defun emcp-tests--parse-http-response-body (response)
  "Parse the JSON body of the HTTP RESPONSE into an alist."
  (json-parse-string (plist-get response :body) :object-type 'alist))

(defun emcp-tests--http-header (response name)
  "Return the value of header NAME in the HTTP RESPONSE."
  (cdr (assoc-string name (plist-get response :headers) t)))

(defmacro emcp-tests-with-http-response (response message &rest body)
  "POST MESSAGE to a test server over HTTP and bind RESPONSE in BODY.

The server has the primitives in `emcp-test-primitives'.  BODY may start
with keyword arguments for `emcp-test-http-request'."
  (declare (indent 2))
  (let (args)
    (while (keywordp (car body))
      (setq args `(,@args ,(pop body) ,(pop body))))
    (let ((server (gensym "server"))
          (conn (gensym "conn")))
      `(emcp-test-with-http-server ,server (apply #'emcp--server-build emcp-test-primitives)
         (emcp-test-with-http-connection ,conn ,server
           (let ((,response (emcp-test-http-request ,conn 'POST ,message ,@args)))
             ,@body))))))

;;; Request routing

(ert-deftest emcp-tests-http-wrong-path ()
  (emcp-tests-with-http-response response (emcp-test-make-request "tools/list")
    :path "/not-mcp"
    (should (= (plist-get response :status) 404))))

(ert-deftest emcp-tests-http-origin-allowed ()
  (emcp-test-with-http-server server (emcp--server-build)
    (dolist (origin '(nil "http://localhost:1234" "http://127.0.0.1" "http://[::1]:8080"))
      (ert-info ((format "Origin: %s" origin))
        (emcp-test-with-http-connection conn server
          (let ((response (emcp-test-http-request
                           conn 'POST (emcp-test-make-request "tools/list")
                           :headers (and origin `(("Origin" . ,origin))))))
            (should (= (plist-get response :status) 200))))))))

(ert-deftest emcp-tests-http-origin-forbidden ()
  (emcp-test-with-http-server server (emcp--server-build)
    (dolist (origin '("http://evil.example" "null"))
      (dolist (method '(POST GET DELETE))
        (ert-info ((format "%s with Origin: %s" method origin))
          (emcp-test-with-http-connection conn server
            (let* ((response (emcp-test-http-request
                              conn method
                              (and (eq method 'POST) (emcp-test-make-request "tools/list"))
                              :headers `(("Origin" . ,origin))))
                   (json (emcp-tests--parse-http-response-body response)))
              (should (= (plist-get response :status) 403))
              (should (equal (emcp-tests--http-header response "Content-Type")
                             "application/json"))
              (should-not (assq 'id json))
              (should (= (alist-get 'code (alist-get 'error json))
                         emcp--jsonrpc-invalid-request)))))))))

(ert-deftest emcp-tests-http-method-not-allowed ()
  (emcp-test-with-http-server server (emcp--server-build)
    (dolist (method '(GET DELETE))
      (ert-info ((symbol-name method))
        (emcp-test-with-http-connection conn server
          (let ((response (emcp-test-http-request conn method nil)))
            (should (= (plist-get response :status) 405))
            (should (equal (emcp-tests--http-header response "Allow") "POST"))))))))

;;; Message parsing

(ert-deftest emcp-tests-http-unparseable-json ()
  (emcp-tests-with-http-response response "{\"jsonrpc\": 2.0, oops}"
    (should (= (plist-get response :status) 400))
    (should (= (alist-get 'code
                          (alist-get 'error (emcp-tests--parse-http-response-body response)))
               emcp--jsonrpc-parse-error))))

(ert-deftest emcp-tests-http-response-from-client ()
  (emcp-tests-with-http-response response '((jsonrpc . "2.0") (id . 1) (result . ((a . 1))))
    (should (= (plist-get response :status) 400))
    (should (= (alist-get 'code
                          (alist-get 'error (emcp-tests--parse-http-response-body response)))
               emcp--jsonrpc-invalid-request))))

(ert-deftest emcp-tests-http-notification ()
  ;; Notifications are accepted with 202 and no body
  (emcp-tests-with-http-response response
      '((jsonrpc . "2.0") (method . "notifications/cancelled") (params . ((requestId . 1))))
    (should (= (plist-get response :status) 202))
    (should (equal (plist-get response :body) ""))))

;;; Legacy clients

(ert-deftest emcp-tests-http-legacy-initialize ()
  ;; A legacy client sends initialize without the new headers and learns the supported
  ;; version from the error.
  (emcp-tests-with-http-response response
      '((jsonrpc . "2.0") (id . 1) (method . "initialize")
        (params . ((protocolVersion . "2025-11-25") (capabilities . ()))))
    :omit-headers '("MCP-Protocol-Version" "Mcp-Method")
    (let ((error (alist-get 'error (emcp-tests--parse-http-response-body response))))
      (should (= (plist-get response :status) 400))
      (should (= (alist-get 'code error) emcp--mcp-unsupported-protocol-version))
      (should (equal (alist-get 'supported (alist-get 'data error)) ["2026-07-28"]))
      (should (equal (alist-get 'requested (alist-get 'data error)) "2025-11-25")))))

(ert-deftest emcp-tests-http-legacy-session-id ()
  ;; The session id of earlier revisions is ignored and not echoed, see the backward
  ;; compatibility section of the Streamable HTTP spec.
  (emcp-tests-with-http-response response (emcp-test-make-request "tools/list")
    :headers '(("Mcp-Session-Id" . "legacy-session"))
    (should (= (plist-get response :status) 200))
    (should-not (emcp-tests--http-header response "Mcp-Session-Id"))))

;;; Mirrored headers

(ert-deftest emcp-tests-http-mirrored-headers-rejected ()
  ;; MCP-Protocol-Version and Mcp-Method are always required, Mcp-Name only for
  ;; tools/call, prompts/get and resources/read.  Each must match the body, otherwise the
  ;; request is rejected with 400 and HeaderMismatch.
  (let ((call (emcp-test-make-request "tools/call" '((name . "tool"))))
        (get (emcp-test-make-request "prompts/get" '((name . "prompt"))))
        (read (emcp-test-make-request "resources/read" '((uri . "emcp://test/static")))))
    (emcp-test-with-http-server server (apply #'emcp--server-build emcp-test-primitives)
      (pcase-dolist (`(,description ,message ,headers ,omit)
                     `(("missing MCP-Protocol-Version" ,call nil ("MCP-Protocol-Version"))
                       ("mismatching MCP-Protocol-Version" ,call
                        (("MCP-Protocol-Version" . "2025-11-25")) nil)
                       ("missing Mcp-Method" ,call nil ("Mcp-Method"))
                       ("mismatching Mcp-Method" ,call (("Mcp-Method" . "tools/list")) nil)
                       ("missing Mcp-Name for tools/call" ,call nil ("Mcp-Name"))
                       ("missing Mcp-Name for prompts/get" ,get nil ("Mcp-Name"))
                       ("missing Mcp-Name for resources/read" ,read nil ("Mcp-Name"))
                       ("mismatching Mcp-Name for tools/call" ,call (("Mcp-Name" . "other")) nil)
                       ("mismatching Mcp-Name for prompts/get" ,get (("Mcp-Name" . "other")) nil)
                       ("mismatching Mcp-Name for resources/read" ,read
                        (("Mcp-Name" . "emcp://test/other")) nil)
                       ("mismatching base64 Mcp-Name" ,call
                        (("Mcp-Name" . "=?base64?b3RoZXI=?=")) nil)
                       ("invalid base64 Mcp-Name" ,call (("Mcp-Name" . "=?base64?!!!?=")) nil)))
        (ert-info (description)
          (emcp-test-with-http-connection conn server
            (let* ((response (emcp-test-http-request conn 'POST message
                                                     :headers headers :omit-headers omit))
                   (json (emcp-tests--parse-http-response-body response)))
              (should (= (plist-get response :status) 400))
              (should (= (alist-get 'id json) 1))
              (should (= (alist-get 'code (alist-get 'error json))
                         emcp--mcp-header-mismatch)))))))))

(emcp-deftool emcp-tests--http-umlaut-tool () "A tool with a non-ASCII name."
              :name "grüße"
              '((content . [((type . "text") (text . "hallo"))])))

(ert-deftest emcp-tests-http-mirrored-headers-accepted ()
  ;; Mcp-Name is only required for tools/call, prompts/get and resources/read, and the
  ;; base64 sentinel is decoded before the comparison.
  (emcp-test-with-http-server server (emcp--server-build
                                      :tools '(emcp-test--tool emcp-tests--http-umlaut-tool)
                                      :prompts '(emcp-test--prompt)
                                      :resources '(emcp-test--resource))
    (pcase-dolist (`(,description ,message ,headers)
                   `(("tools/list without Mcp-Name" ,(emcp-test-make-request "tools/list") nil)
                     ("server/discover without Mcp-Name"
                      ,(emcp-test-make-request "server/discover") nil)
                     ("tools/call" ,(emcp-test-make-request "tools/call" '((name . "tool"))) nil)
                     ("prompts/get" ,(emcp-test-make-request "prompts/get" '((name . "prompt")))
                      nil)
                     ("resources/read"
                      ,(emcp-test-make-request "resources/read" '((uri . "emcp://test/static")))
                      nil)
                     ("base64 non-ASCII name"
                      ,(emcp-test-make-request "tools/call" '((name . "grüße")))
                      (("Mcp-Name" . "=?base64?Z3LDvMOfZQ==?=")))
                     ("base64 ASCII name"
                      ,(emcp-test-make-request "tools/call" '((name . "tool")))
                      (("Mcp-Name" . "=?base64?dG9vbA==?=")))))
      (ert-info (description)
        (emcp-test-with-http-connection conn server
          (let ((response (emcp-test-http-request conn 'POST message :headers headers)))
            (should (= (plist-get response :status) 200))
            (should (alist-get 'result (emcp-tests--parse-http-response-body response)))))))))

;;; Request handling

(ert-deftest emcp-tests-http-valid-request ()
  (emcp-tests-with-http-response response (emcp-test-make-request "tools/list")
    (should (= (plist-get response :status) 200))
    (should (equal (emcp-tests--http-header response "Content-Type") "application/json"))
    (let ((json (emcp-tests--parse-http-response-body response)))
      (should (= (alist-get 'id json) 1))
      (should (equal (alist-get 'resultType (alist-get 'result json)) "complete")))))

(ert-deftest emcp-tests-http-unsupported-version ()
  (emcp-tests-with-http-response response
      (emcp-test-make-request "tools/list" nil :protocol-version "2025-11-25")
    (let ((error (alist-get 'error (emcp-tests--parse-http-response-body response))))
      (should (= (plist-get response :status) 400))
      (should (= (alist-get 'code error) emcp--mcp-unsupported-protocol-version))
      (should (equal (alist-get 'supported (alist-get 'data error)) ["2026-07-28"])))))

(ert-deftest emcp-tests-http-missing-meta ()
  (emcp-tests-with-http-response response
      '((jsonrpc . "2.0") (id . 1) (method . "tools/list"))
    (should (= (plist-get response :status) 400))
    (should (= (alist-get 'code
                          (alist-get 'error (emcp-tests--parse-http-response-body response)))
               emcp--jsonrpc-invalid-params))))

(ert-deftest emcp-tests-http-unknown-method ()
  (emcp-tests-with-http-response response (emcp-test-make-request "tools/unknown")
    (should (= (plist-get response :status) 404))
    (should (= (alist-get 'code
                          (alist-get 'error (emcp-tests--parse-http-response-body response)))
               emcp--jsonrpc-method-not-found))))

(provide 'emcp-http-tests)
;;; emcp-http-tests.el ends here
