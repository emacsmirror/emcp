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

(ert-deftest emcp-tests-http-wrong-path ()
  (emcp-test-with-http-server server (emcp--server-build)
    (emcp-test-with-http conn server
      (let ((response (emcp-test-http-request
                       conn 'POST '((jsonrpc . "2.0") (id . 1) (method . "ping"))
                       :path "/not-mcp")))
        (should (= (plist-get response :status) 404))))))

(ert-deftest emcp-tests-http-unparseable-json ()
  (emcp-test-with-http-server server (emcp--server-build)
    (emcp-test-with-http conn server
      ;; Not truncated JSON, since emcp-http does not catch `json-end-of-file' once json.el
      ;; has redefined it
      (let ((response (emcp-test-http-request conn 'POST "{\"jsonrpc\": 2.0, oops}")))
        (should (= (plist-get response :status) 400))))))

(ert-deftest emcp-tests-http-origin-allowed ()
  (emcp-test-with-http-server server (emcp--server-build)
    (dolist (origin '(nil "http://localhost:1234" "http://127.0.0.1" "http://[::1]:8080"))
      (ert-info ((format "Origin: %s" origin))
        (emcp-test-with-http conn server
          (let ((response (emcp-test-http-request
                           conn 'POST '((jsonrpc . "2.0") (id . 1) (method . "initialize")
                                        (params . ((protocolVersion . "2025-11-25"))))
                           :headers (and origin `(("Origin" . ,origin))))))
            (should (= (plist-get response :status) 201))))))))

(ert-deftest emcp-tests-http-origin-forbidden ()
  (emcp-test-with-http-server server (emcp--server-build)
    (dolist (origin '("http://evil.example" "null"))
      (dolist (method '(POST GET DELETE))
        (ert-info ((format "%s with Origin: %s" method origin))
          (emcp-test-with-http conn server
            (let* ((response (emcp-test-http-request
                              conn method
                              (and (eq method 'POST)
                                   '((jsonrpc . "2.0") (id . 1) (method . "ping")))
                              :headers `(("Origin" . ,origin))))
                   (json (json-parse-string (plist-get response :body) :object-type 'alist)))
              (should (= (plist-get response :status) 403))
              (should (equal (cdr (assoc-string "Content-Type" (plist-get response :headers) t))
                             "application/json"))
              (should-not (assq 'id json))
              (should (= (alist-get 'code (alist-get 'error json))
                         emcp--jsonrpc-invalid-request)))))))))

(provide 'emcp-http-tests)
;;; emcp-http-tests.el ends here
