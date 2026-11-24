;;; emcp-core-tests.el --- Tests for the core MCP server -*- lexical-binding: t -*-

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

;; Tests for the primitive macros and the request handling in emcp-core.el.

;;; Code:

(require 'emcp-core)
(require 'emcp-test-helpers)
(require 'ert)

;;; Helpers

(defun emcp-tests--error-code (response)
  "Return the error code of RESPONSE."
  (alist-get 'code (alist-get 'error response)))

;;; Prompts

(ert-deftest emcp-tests-sync-prompt ()
  (let ((sym (make-symbol "prompt")))
    (eval
     `(emcp-defprompt ,sym ((name "User name") (lang "Language" :default "German"))
        "A simple test prompt."
        :name "hello"
        :title "say hello"
        :description "Greets the user."
        `((description . ,(format "A pretend greeting in %s" lang))
          (messages . [((role . "user")
                        (content . ((type . "text")
                                    (text . ,(format "Hello, I am %s. Pretend this was in %s." name lang)))))])))
     t)

    (emcp-test-with-server server (emcp--server-build :prompts (list sym))
      (let ((client (emcp-test-make-client :server server)))
        (emcp-test-client-with-response client response '((method . "prompts/list"))
          (should (equal (emcp-test-response-data response)
                         '((prompts . [((name . "hello")
                                        (title . "say hello")
                                        (description . "Greets the user.")
                                        (arguments . [((name . "name")
                                                       (description . "User name")
                                                       (required . t))
                                                      ((name . "lang")
                                                       (description . "Language")
                                                       (required . :false))]))])))))

        (emcp-test-client-with-response client response
                                        '((method . "prompts/get")
                                          (params . ((name . "hello")
                                                     (arguments . ((name . "Emacs")
                                                                   (lang . "Spanish"))))))
          (should (equal (emcp-test-response-data response)
                         '((description . "A pretend greeting in Spanish")
                           (messages . [((role . "user")
                                         (content . ((type . "text")
                                                     (text . "Hello, I am Emacs. Pretend this was in Spanish."))))])))))))))

(ert-deftest emcp-tests-sync-prompt-minimal ()
  (let ((sym (make-symbol "minimal-prompt")))
    (eval
     `(emcp-defprompt ,sym ((name) lang)
        "A minimal prompt."
        `((description . ,(format "A pretend greeting in %s" lang))
          (messages . [((role . "user")
                        (content . ((type . "text")
                                    (text . ,(format "Hello, I am %s. Pretend this was in %s." name lang)))))])))
     t)

    (emcp-test-with-server server (emcp--server-build :prompts (list sym))
      (let ((client (emcp-test-make-client :server server)))
        (emcp-test-client-with-response client response '((method . "prompts/list"))
          (should (equal (emcp-test-response-data response)
                         `((prompts . [((name . ,(symbol-name sym))
                                        (description . "A minimal prompt.")
                                        (arguments . [((name . "name")
                                                       (required . t))
                                                      ((name . "lang")
                                                       (required . t))]))])))))

        (emcp-test-client-with-response client response
                                        `((method . "prompts/get")
                                          (params . ((name . ,(symbol-name sym))
                                                     (arguments . ((name . "Emacs")
                                                                   (lang . "Spanish"))))))
          (should (equal (emcp-test-response-data response)
                         '((description . "A pretend greeting in Spanish")
                           (messages . [((role . "user")
                                         (content . ((type . "text")
                                                     (text . "Hello, I am Emacs. Pretend this was in Spanish."))))])))))))))

(ert-deftest emcp-tests-async-prompt ()
  (let ((sym (make-symbol "async-prompt")))
    (eval
     `(emcp-defprompt ,sym ((name) lang)
        "An async prompt."
        :async t
        (if (equal lang "English")
            (send-error 1000 "Cannot speak language" `((language . ,lang)))
          (send-result `((description . ,(format "A pretend greeting in %s" lang))
                         (messages . [((role . "user")
                                       (content . ((type . "text")
                                                   (text . ,(format "Hello, I am %s. Pretend this was in %s." name lang)))))])))))
     t)

    (emcp-test-with-server server (emcp--server-build :prompts (list sym))
      (let ((client (emcp-test-make-client :server server)))
        (emcp-test-client-with-response client response
                                        `((method . "prompts/get")
                                          (params . ((name . ,(symbol-name sym))
                                                     (arguments . ((name . "Emacs")
                                                                   (lang . "Spanish"))))))
          (should (equal (emcp-test-response-data response)
                         '((description . "A pretend greeting in Spanish")
                           (messages . [((role . "user")
                                         (content . ((type . "text")
                                                     (text . "Hello, I am Emacs. Pretend this was in Spanish."))))])))))

        (emcp-test-client-with-response client response
                                        `((method . "prompts/get")
                                          (params . ((name . ,(symbol-name sym))
                                                     (arguments . ((name . "Emacs")
                                                                   (lang . "English"))))))
          (should (equal (alist-get 'error response)
                         `((code . 1000)
                           (message . "Cannot speak language")
                           (data . ((language . "English")))))))))))

;;; Tools

(ert-deftest emcp-tests-sync-tool ()
  (let ((sym (make-symbol "addition-tool")))
    (eval
     `(emcp-deftool ,sym ((name "Who is performing the addition" :default "Emacs")
                          (a "One of the numbers to add" :type "number")
                          (b :type "number" :default 10))
        "An addition tool."
        :name "magic-add"
        :title "Add things"
        :description "Somebody is performing addition"
        `((content . [((type . "text")
                       (text . ,(format "%s says %s" name (+ a b))))])))
     t)

    (emcp-test-with-server server (emcp--server-build :tools (list sym))
      (let ((client (emcp-test-make-client :server server)))
        (emcp-test-client-with-response client response '((method . "tools/list"))
          (should (equal (emcp-test-response-data response)
                         '((tools . [((name . "magic-add")
                                      (title . "Add things")
                                      (description . "Somebody is performing addition")
                                      (inputSchema . ((type . "object")
                                                      (properties . ((name . ((type . "string")
                                                                              (description . "Who is performing the addition")))
                                                                     (a . ((type . "number")
                                                                           (description . "One of the numbers to add")))
                                                                     (b . ((type . "number")))))
                                                      (required . ["a"]))))])))))

        (emcp-test-client-with-response client response
                                        '((method . "tools/call")
                                          (params . ((name . "magic-add")
                                                     (arguments . ((a . 10))))))
          (should (equal (emcp-test-response-data response)
                         '((content . [((type . "text")
                                        (text . "Emacs says 20"))])))))))))

(ert-deftest emcp-tests-sync-tool-minimal ()
  (let ((sym (make-symbol "minimal-tool")))
    (eval
     `(emcp-deftool ,sym (name (a :type "number") (b :type "number"))
        "An addition tool."
        `((content . [((type . "text")
                       (text . ,(format "%s says %s" name (+ a b))))])))
     t)

    (emcp-test-with-server server (emcp--server-build :tools (list sym))
      (let ((client (emcp-test-make-client :server server)))
        (emcp-test-client-with-response client response '((method . "tools/list"))
          (should (equal (emcp-test-response-data response)
                         `((tools . [((name . ,(symbol-name sym))
                                      (description . "An addition tool.")
                                      (inputSchema . ((type . "object")
                                                      (properties . ((name . ((type . "string")))
                                                                     (a . ((type . "number")))
                                                                     (b . ((type . "number")))))
                                                      (required . ["name" "a" "b"]))))])))))))))

(ert-deftest emcp-tests-async-tool ()
  (let ((sym (make-symbol "async-tool")))
    (eval
     `(emcp-deftool ,sym (name (a :type "number") (b :type "number"))
        "An addition tool."
        :async t
        (if (equal name "Emacs")
            (send-result `((content . [((type . "text")
                                        (text . ,(format "%s says %s" name (+ a b))))])))
          (send-error 1001 "Addition is hard" `((name . ,name)))))
     t)

    (emcp-test-with-server server (emcp--server-build :tools (list sym))
      (let ((client (emcp-test-make-client :server server)))
        (emcp-test-client-with-response client response
                                        `((method . "tools/call")
                                          (params . ((name . ,(symbol-name sym))
                                                     (arguments . ((name . "Emacs")
                                                                   (a . 10)
                                                                   (b . 5))))))
          (should (equal (emcp-test-response-data response)
                         '((content . [((type . "text")
                                        (text . "Emacs says 15"))])))))

        (emcp-test-client-with-response client response
                                        `((method . "tools/call")
                                          (params . ((name . ,(symbol-name sym))
                                                     (arguments . ((name . "Gnu")
                                                                   (a . 10)
                                                                   (b . 5))))))
          (should (equal (alist-get 'error response)
                         `((code . 1001)
                           (message . "Addition is hard")
                           (data . ((name . "Gnu")))))))))))

(ert-deftest emcp-tests-tools-list-order ()
  ;; tools/list follows the profile order, on every call and after a
  ;; reload.
  (let ((syms (mapcar #'make-symbol '("zeta" "alpha" "mid"))))
    (dolist (sym syms)
      (eval `(emcp-deftool ,sym () "A tool." '((content . []))) t))
    (cl-flet ((names (client)
                (mapcar (lambda (tool) (alist-get 'name tool))
                        (alist-get 'tools (emcp-test-response-data
                                           (emcp-test-client-request
                                            client "tools/list"))))))
      (emcp-test-with-server server (emcp--server-build :tools syms)
        (let ((client (emcp-test-make-client :server server)))
          (should (equal (names client) '("zeta" "alpha" "mid")))
          (should (equal (names client) '("zeta" "alpha" "mid")))
          (emcp--server-reload server :tools (reverse syms))
          (should (equal (names client) '("mid" "alpha" "zeta"))))))))

;;; Resources

(ert-deftest emcp-tests-static-resource ()
  (let ((sym (make-symbol "static-resource")))
    (eval
     `(emcp-defresource ,sym "emcp://test/greeting"
        "A static greeting resource."
        :name "greeting"
        `((contents . [((uri . ,uri)
                        (mimeType . "text/plain")
                        (text . "Hello from EMCP"))])))
     t)

    (emcp-test-with-server server (emcp--server-build :resources (list sym))
      (let ((client (emcp-test-make-client :server server)))
        ;; Should appear in resources/list, not templates/list
        (emcp-test-client-with-response client response
                                        '((method . "resources/list"))
          (let ((resources (alist-get 'resources (emcp-test-response-data response))))
            (should (= (length resources) 1))
            (should (equal (alist-get 'uri (aref resources 0))
                           "emcp://test/greeting"))))

        (emcp-test-client-with-response client response
                                        '((method . "resources/templates/list"))
          (should (equal (alist-get 'resourceTemplates (emcp-test-response-data response))
                         [])))

        ;; Should be readable
        (emcp-test-client-with-response client response
                                        '((method . "resources/read")
                                          (params . ((uri . "emcp://test/greeting"))))
          (let* ((contents (alist-get 'contents (emcp-test-response-data response)))
                 (text (alist-get 'text (aref contents 0))))
            (should (equal text "Hello from EMCP"))))))))

(ert-deftest emcp-tests-async-resource-template ()
  (let ((sym (make-symbol "async-resource")))
    (eval
     `(emcp-defresource ,sym "emcp://test/{name}"
        "An async greeting resource."
        :name "greeting"
        :async t
        (if (equal name "fail")
            (send-error -1 "No greeting for you")
          (send-result `((contents . [((uri . ,uri)
                                       (mimeType . "text/plain")
                                       (text . ,(format "Hello, %s" name)))])))))
     t)

    (emcp-test-with-server server (emcp--server-build :resources (list sym))
      (let ((client (emcp-test-make-client :server server)))
        ;; Successful read
        (emcp-test-client-with-response client response
                                        '((method . "resources/read")
                                          (params . ((uri . "emcp://test/world"))))
          (let* ((contents (alist-get 'contents (emcp-test-response-data response)))
                 (text (alist-get 'text (aref contents 0))))
            (should (equal text "Hello, world"))))

        ;; Error path
        (emcp-test-client-with-response client response
                                        '((method . "resources/read")
                                          (params . ((uri . "emcp://test/fail"))))
          (should (alist-get 'error response))
          (should (equal (alist-get 'message (alist-get 'error response))
                         "No greeting for you")))))))

(ert-deftest emcp-tests-resource-read-errors ()
  ;; A URI that no resource matches and a body that signals
  ;; `emcp-resource-not-found' return invalid params, any other error
  ;; that a body signals returns an internal error.
  (let ((missing (make-symbol "missing"))
        (broken (make-symbol "broken")))
    (eval `(progn
             (emcp-defresource ,missing "emcp://test/missing" "A missing resource."
                               (signal 'emcp-resource-not-found nil))
             (emcp-defresource ,broken "emcp://test/broken" "A broken resource."
                               (error "Broken")))
          t)
    (emcp-test-with-server server (emcp--server-build :resources (list missing broken))
      (let ((client (emcp-test-make-client :server server)))
        (pcase-dolist (`(,uri ,code)
                       `(("emcp://test/unknown" ,emcp--jsonrpc-invalid-params)
                         ("emcp://test/missing" ,emcp--jsonrpc-invalid-params)
                         ("emcp://test/broken" ,emcp--jsonrpc-internal-error)))
          (ert-info (uri)
            (should (= (emcp-tests--error-code
                        (emcp-test-client-request
                         client "resources/read" `((uri . ,uri))))
                       code))))))))

;;; Request context

(defun emcp-tests--describe-context (context)
  "Summarize CONTEXT as a string."
  (format "%s %s %s %s"
          (gethash "name" (emcp-context-client-info context))
          (emcp-context-protocol-version context)
          (emcp-context-request-id context)
          (emcp--server-name (emcp-context-server context))))

(ert-deftest emcp-tests-bodies-see-context ()
  ;; Tool, prompt and resource bodies all see the request context.
  (let ((tool (make-symbol "context-tool"))
        (prompt (make-symbol "context-prompt"))
        (resource (make-symbol "context-resource")))
    (eval
     `(progn
        (emcp-deftool ,tool ()
          "A tool describing its context."
          `((content . [((type . "text") (text . ,(emcp-tests--describe-context context)))])))
        (emcp-defprompt ,prompt ()
          "A prompt describing its context."
          `((messages . [((role . "user")
                          (content . ((type . "text")
                                      (text . ,(emcp-tests--describe-context context)))))])))
        (emcp-defresource ,resource "emcp://test/context"
          "A resource describing its context."
          `((contents . [((uri . ,uri) (text . ,(emcp-tests--describe-context context)))]))))
     t)
    (emcp-test-with-server server (emcp--server-build :name "emcp-test"
                                                      :tools (list tool)
                                                      :prompts (list prompt)
                                                      :resources (list resource))
      (let ((client (emcp-test-make-client
                     :server server :client-info '((name . "test-client") (version . "1.0")))))
        (emcp-test-client-with-response client response
                                        `((id . 42)
                                          (method . "tools/call")
                                          (params . ((name . ,(symbol-name tool)))))
          (let ((content (alist-get 'content (emcp-test-response-data response))))
            (should (equal (alist-get 'text (aref content 0))
                           "test-client 2026-07-28 42 emcp-test"))))
        (emcp-test-client-with-response client response
                                        `((id . 43)
                                          (method . "prompts/get")
                                          (params . ((name . ,(symbol-name prompt)))))
          (let ((message (aref (alist-get 'messages (emcp-test-response-data response))
                               0)))
            (should (equal (alist-get 'text (alist-get 'content message))
                           "test-client 2026-07-28 43 emcp-test"))))
        (emcp-test-client-with-response client response
                                        '((id . 44)
                                          (method . "resources/read")
                                          (params . ((uri . "emcp://test/context"))))
          (let ((contents (alist-get 'contents (emcp-test-response-data response))))
            (should (equal (alist-get 'text (aref contents 0))
                           "test-client 2026-07-28 44 emcp-test"))))))))

(ert-deftest emcp-tests-log-prefix-client-label ()
  ;; Log lines start with the client name, truncated or padded to a fixed width.
  (emcp-test-with-server server (emcp--server-build)
    (dolist (name '("a-very-long-client-name" "short"))
      (let ((client (emcp-test-make-client
                     :server server :client-info `((name . ,name) (version . "1.0")))))
        (emcp-test-client-request client "tools/list")))
    (with-current-buffer (emcp--server-log-buffer server)
      (let ((log (buffer-string)))
        (should (string-match-p "^[^ ]+ [^ ]+ INFO  a-very-lon > Request tools/list" log))
        (should (string-match-p "^[^ ]+ [^ ]+ INFO  short      > Request tools/list" log))))))

;;; Request validation

(defun emcp-tests--tools-list-request-context (server &rest meta-args)
  "Return the context of a tools/list request to SERVER.

META-ARGS override or omit _meta fields, see `emcp-test-make-request-meta'."
  (emcp--request-context
   server (emcp-test-to-hash-table (apply #'emcp-test-make-request "tools/list" nil meta-args))))

(ert-deftest emcp-tests-request-missing-meta-field ()
  ;; Both required _meta fields are checked; a malformed field counts as missing.
  (emcp-test-with-server server (emcp--server-build)
    (dolist (args '((:omit (:protocol-version))
                    (:omit (:client-capabilities))
                    (:protocol-version 20260728)
                    (:client-capabilities "none")))
      (ert-info ((format "%S" args))
        (pcase-let ((`(,_ ,code . ,_)
                     (should-error (apply #'emcp-tests--tools-list-request-context server args)
                                   :type 'emcp-invalid-request)))
          (should (= code emcp--jsonrpc-invalid-params)))))))

(ert-deftest emcp-tests-request-unsupported-version ()
  (emcp-test-with-server server (emcp--server-build)
    (pcase-let ((`(,_ ,code ,_ ,data)
                 (should-error (emcp-tests--tools-list-request-context
                                server :protocol-version "2025-11-25")
                               :type 'emcp-invalid-request)))
      (should (= code emcp--mcp-unsupported-protocol-version))
      (should (equal data '((supported . ["2026-07-28"]) (requested . "2025-11-25")))))))

(ert-deftest emcp-tests-request-log-level ()
  ;; A log level from the spec is accepted, any other value is invalid
  ;; params.
  (emcp-test-with-server server (emcp--server-build)
    (should (emcp-tests--tools-list-request-context server :log-level "debug"))
    (pcase-let ((`(,_ ,code . ,_)
                 (should-error (emcp-tests--tools-list-request-context server :log-level "verbose")
                               :type 'emcp-invalid-request)))
      (should (= code emcp--jsonrpc-invalid-params)))))

(ert-deftest emcp-tests-unknown-method ()
  (emcp-test-with-server server (emcp--server-build)
    (let ((client (emcp-test-make-client :server server)))
      (should (= (emcp-tests--error-code
                  (emcp-test-client-request client "tools/unknown"))
                 emcp--jsonrpc-method-not-found)))))

(ert-deftest emcp-tests-unknown-tool-or-prompt ()
  ;; Unknown names are invalid params, see the tools and prompts error
  ;; handling in the spec
  (emcp-test-with-server server (apply #'emcp--server-build emcp-test-primitives)
    (let ((client (emcp-test-make-client :server server)))
      (pcase-dolist (`(,method ,message) '(("tools/call" "Unknown tool: missing")
                                           ("prompts/get" "Unknown prompt: missing")))
        (ert-info (method)
          (let ((error (alist-get 'error (emcp-test-client-request
                                          client method '((name . "missing"))))))
            (should (= (alist-get 'code error) emcp--jsonrpc-invalid-params))
            (should (equal (alist-get 'message error) message))))))))

;;; Results

(defconst emcp-tests--all-requests
  '(("server/discover")
    ("tools/list")
    ("tools/call" ((name . "tool")))
    ("prompts/list")
    ("prompts/get" ((name . "prompt")))
    ("resources/list")
    ("resources/templates/list")
    ("resources/read" ((uri . "emcp://test/static"))))
  "One (METHOD PARAMS) request for each method that EMCP implements.")

(ert-deftest emcp-tests-results-complete-with-server-info ()
  (emcp-test-with-server server (apply #'emcp--server-build emcp-test-primitives)
    (let ((client (emcp-test-make-client :server server)))
      (pcase-dolist (`(,method ,params) emcp-tests--all-requests)
        (ert-info (method)
          (let* ((response (emcp-test-client-request client method params))
                 (result (alist-get 'result response))
                 (info (alist-get 'io.modelcontextprotocol/serverInfo (alist-get '_meta result))))
            (should (equal (alist-get 'resultType result) "complete"))
            (should (equal (alist-get 'name info) "EMCP"))
            (should (stringp (alist-get 'version info)))))))))

(ert-deftest emcp-tests-result-meta-merged ()
  ;; A body's own _meta entries are kept next to the server info.
  (let ((sym (make-symbol "meta-tool")))
    (eval `(emcp-deftool ,sym () "A tool with _meta."
                         '((content . []) (_meta . ((com.example/trace . "abc")))))
          t)
    (emcp-test-with-server server (emcp--server-build :tools (list sym))
      (let* ((client (emcp-test-make-client :server server))
             (result (alist-get 'result (emcp-test-client-request client "tools/call"
                                                                  `((name . ,(symbol-name sym))))))
             (meta (alist-get '_meta result)))
        (should (equal (alist-get 'com.example/trace meta) "abc"))
        (should (equal (alist-get 'name (alist-get 'io.modelcontextprotocol/serverInfo meta))
                       "EMCP"))))))

(ert-deftest emcp-tests-cache-hints ()
  ;; Lists and discover are public for `emcp-list-ttl-ms', resource
  ;; reads private for their :ttl-ms.
  (emcp-test-with-server server (apply #'emcp--server-build emcp-test-primitives)
    (let ((client (emcp-test-make-client :server server))
          (emcp-list-ttl-ms 1234))
      (pcase-dolist (`(,method ,params ,ttl ,scope)
                     '(("server/discover" nil 1234 "public")
                       ("tools/list" nil 1234 "public")
                       ("prompts/list" nil 1234 "public")
                       ("resources/list" nil 1234 "public")
                       ("resources/templates/list" nil 1234 "public")
                       ("resources/read" ((uri . "emcp://test/static")) 0 "private")
                       ("resources/read" ((uri . "emcp://test/template/1")) 500 "private")))
        (ert-info ((format "%s %S" method params))
          (let* ((response (emcp-test-client-request client method params))
                 (result (alist-get 'result response)))
            (should (equal (alist-get 'ttlMs result) ttl))
            (should (equal (alist-get 'cacheScope result) scope))))))))

(ert-deftest emcp-tests-discover ()
  (emcp-test-with-server server (apply #'emcp--server-build emcp-test-primitives)
    (let* ((client (emcp-test-make-client :server server))
           (emcp-instructions "Test instructions")
           (response (emcp-test-client-request client "server/discover"))
           (result (alist-get 'result response))
           (capabilities (alist-get 'capabilities result))
           (info (alist-get 'io.modelcontextprotocol/serverInfo (alist-get '_meta result))))
      (should (equal (alist-get 'supportedVersions result) ["2026-07-28"]))
      (should (assq 'tools capabilities))
      (should (assq 'prompts capabilities))
      (should (assq 'resources capabilities))
      (should (equal (alist-get 'instructions result) "Test instructions"))
      (should (equal (alist-get 'name info) "EMCP"))
      (should (string-prefix-p "data:image/svg+xml;base64,"
                               (alist-get 'src (aref (alist-get 'icons info) 0)))))))

(provide 'emcp-core-tests)
;;; emcp-core-tests.el ends here
