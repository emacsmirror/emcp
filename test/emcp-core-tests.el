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

;; Tests for the capability macros and the request handling in emcp-core.el.

;;; Code:

(require 'emcp-core)
(require 'emcp-test-helpers)
(require 'ert)

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
                                    (text . ,(format "Hello, I am %s. Pretend this was in %s." name lang)))))]))))

    (emcp-test-with-server server (emcp--server-build :prompts (list sym))
      (let ((client (emcp-test-client-init-session server)))
        (emcp-test-client-with-response client response '((method . "prompts/list"))
          (should (equal (alist-get 'result response)
                         '((prompts . [((name . "hello")
                                        (title "say hello")
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
          (should (equal (alist-get 'result response)
                         `((description . ,(format "A pretend greeting in Spanish" lang))
                           (messages . [((role . "user")
                                         (content . ((type . "text")
                                                     (text . "Hello, I am Emacs. Pretend this was in Spanish."))))])))))))))

(ert-deftest emcp-tests-sync-prompt-with-defaults ()
  (let ((sym (make-symbol "async-prompt")))
    (eval
     `(emcp-defprompt ,sym ((name) lang)
        "A minimal prompt."
        `((description . ,(format "A pretend greeting in %s" lang))
          (messages . [((role . "user")
                        (content . ((type . "text")
                                    (text . ,(format "Hello, I am %s. Pretend this was in %s." name lang)))))]))))

    (emcp-test-with-server server (emcp--server-build :prompts (list sym))
      (let ((client (emcp-test-client-init-session server)))
        (emcp-test-client-with-response client response
                                        `((method . "prompts/get")
                                          (params . ((name . ,(symbol-name sym))
                                                     (arguments . ((name . "Emacs")
                                                                   (lang . "Spanish"))))))
          (should (equal (alist-get 'result response)
                         `((description . ,(format "A pretend greeting in Spanish" lang))
                           (messages . [((role . "user")
                                         (content . ((type . "text")
                                                     (text . "Hello, I am Emacs. Pretend this was in Spanish."))))])))))))))

(ert-deftest emcp-tests-async-prompt ()
  (let ((sym (make-symbol "sync-prompt")))
    (eval
     `(emcp-defprompt ,sym ((name) lang)
        "A minimal prompt."
        :async t
        (if (equal lang "English")
            (send-error 1000 "Cannot speak language" `((language . ,lang)))
          (send-result `((description . ,(format "A pretend greeting in %s" lang))
                         (messages . [((role . "user")
                                       (content . ((type . "text")
                                                   (text . ,(format "Hello, I am %s. Pretend this was in %s." name lang)))))]))))))

    (emcp-test-with-server server (emcp--server-build :prompts (list sym))
      (let ((client (emcp-test-client-init-session server)))
        (emcp-test-client-with-response client response
                                        `((method . "prompts/get")
                                          (params . ((name . ,(symbol-name sym))
                                                     (arguments . ((name . "Emacs")
                                                                   (lang . "Spanish"))))))
          (should (equal (alist-get 'result response)
                         `((description . ,(format "A pretend greeting in Spanish" lang))
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
                       (text . ,(format "%s says %s" name (+ a b))))]))))

    (emcp-test-with-server server (emcp--server-build :tools (list sym))
      (let ((client (emcp-test-client-init-session server)))
        (emcp-test-client-with-response client response '((method . "tools/list"))
          (should (equal (alist-get 'result response)
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
          (should (equal (alist-get 'result response)
                         '((content . [((type . "text")
                                        (text . "Emacs says 20"))])))))))))

(ert-deftest emcp-tests-sync-tool-with-defaults ()
  (let ((sym (make-symbol "add-tool")))
    (eval
     `(emcp-deftool ,sym (name (a :type "number") (b :type "number"))
        "An addition tool."
        `((content . [((type . "text")
                       (text . ,(format "%s says %s" name (+ a b))))]))))

    (emcp-test-with-server server (emcp--server-build :tools (list sym))
      (let ((client (emcp-test-client-init-session server)))
        (emcp-test-client-with-response client response '((method . "tools/list"))
          (should (equal (alist-get 'result response)
                         `((tools . [((name . ,(symbol-name sym))
                                      (description . "An addition tool.")
                                      (inputSchema . ((type . "object")
                                                      (properties . ((name . ((type . "string")))
                                                                     (a . ((type . "number")))
                                                                     (b . ((type . "number")))))
                                                      (required . ["name" "a" "b"]))))])))))))))

(ert-deftest emcp-tests-async-tool ()
  (let ((sym (make-symbol "sync-prompt")))
    (eval
     `(emcp-deftool ,sym (name (a :type "number") (b :type "number"))
        "An addition tool."
        :async t
        (if (equal name "Emacs")
            (send-result `((content . [((type . "text")
                                        (text . ,(format "%s says %s" name (+ a b))))])))
          (send-error 1001 "Addition is hard" `((name . ,name))))))

    (emcp-test-with-server server (emcp--server-build :tools (list sym))
      (let ((client (emcp-test-client-init-session server)))
        (emcp-test-client-with-response client response
                                        `((method . "tools/call")
                                          (params . ((name . ,(symbol-name sym))
                                                     (arguments . ((name . "Emacs")
                                                                   (a . 10)
                                                                   (b . 5))))))
          (should (equal (alist-get 'result response)
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

;;; Resources

(ert-deftest emcp-tests-static-resource ()
  (let ((sym (make-symbol "static-resource")))
    (eval
     `(emcp-defresource ,sym "emcp://test/greeting"
        "A static greeting resource."
        :name "greeting"
        `((contents . [((uri . ,uri)
                        (mimeType . "text/plain")
                        (text . "Hello from EMCP"))]))))

    (emcp-test-with-server server (emcp--server-build :resources (list sym))
      (let ((client (emcp-test-client-init-session server)))
        ;; Should appear in resources/list, not templates/list
        (emcp-test-client-with-response client response
                                        '((method . "resources/list"))
          (let ((resources (alist-get 'resources (alist-get 'result response))))
            (should (= (length resources) 1))
            (should (equal (alist-get 'uri (aref resources 0))
                           "emcp://test/greeting"))))

        (emcp-test-client-with-response client response
                                        '((method . "resources/templates/list"))
          (should (equal (alist-get 'resourceTemplates (alist-get 'result response))
                         [])))

        ;; Should be readable
        (emcp-test-client-with-response client response
                                        '((method . "resources/read")
                                          (params . ((uri . "emcp://test/greeting"))))
          (let ((text (alist-get 'text (aref (alist-get 'contents
                                                        (alist-get 'result response)) 0))))
            (should (equal text "Hello from EMCP"))))))))

(ert-deftest emcp-tests-async-resource ()
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
                                       (text . ,(format "Hello, %s" name)))]))))))

    (emcp-test-with-server server (emcp--server-build :resources (list sym))
      (let ((client (emcp-test-client-init-session server)))
        ;; Successful read
        (emcp-test-client-with-response client response
                                        '((method . "resources/read")
                                          (params . ((uri . "emcp://test/world"))))
          (let ((text (alist-get 'text (aref (alist-get 'contents
                                                        (alist-get 'result response)) 0))))
            (should (equal text "Hello, world"))))

        ;; Error path
        (emcp-test-client-with-response client response
                                        '((method . "resources/read")
                                          (params . ((uri . "emcp://test/fail"))))
          (should (alist-get 'error response))
          (should (equal (alist-get 'message (alist-get 'error response))
                         "No greeting for you")))))))

;;; Server requests & notifications

(ert-deftest emcp-tests-send-server-notification ()
  (emcp-test-with-server server (emcp--server-build)
    (let* (notification
           (client (emcp-test-client-init-session
                    server
                    :on-notification (lambda (n) (setq notification n))))
           (session (emcp-test-client-session client)))
      (emcp--server-send-notification
       server session "test/notification"
       :params '((name . "emcp")))
      (should (equal notification
                     '((jsonrpc . "2.0")
                       (method . "test/notification")
                       (params . ((name . "emcp")))))))))

(ert-deftest emcp-tests-send-server-request ()
  (emcp-test-with-server server (emcp--server-build)
    (let* (request
           (client (emcp-test-client-init-session
                    server
                    :on-request (lambda (r) (setq request r))))
           (session (emcp-test-client-session client))
           result error)
      (emcp--server-send-request
       server session "test/succeed"
       :params '((name . "emcp"))
       :on-result (lambda (r) (setq result r))
       :on-error (lambda (code message data)
                   (setq error (list code message data))))
      (should (equal request
                     '((jsonrpc . "2.0")
                       (method . "test/succeed")
                       (params . ((name . "emcp")))
                       (id . 1))))
      (emcp--server-on-result server session (emcp-test-to-hash-table '((jsonrpc . "2.0")
                                                                        (result . "success")
                                                                        (id . 1))))
      (should (equal result "success"))
      (should (not error))
      (setq result nil
            error nil)
      (emcp--server-send-request
       server session "test/fail"
       :on-result (lambda (r) (setq result r))
       :on-error (lambda (code message data)
                   (setq error (list code message data))))
      (should (equal request
                     '((jsonrpc . "2.0")
                       (method . "test/fail")
                       (id . 2))))
      (emcp--server-on-error server session (emcp-test-to-hash-table '((jsonrpc . "2.0")
                                                                       (error . ((code . 128)
                                                                                 (message . "miserably")
                                                                                 (data . "some data")))
                                                                       (id . 2))))
      (should (not result))
      (should (equal error '(128 "miserably" "some data"))))))

;;; Roots and session labels

(ert-deftest emcp-tests-fetch-roots-on-initialized ()
  ;; A client that declares "roots" capability receives a "roots/list" request right after
  ;; "notifications/initialized", and the normalized response is stored on the session.
  (emcp-test-with-server server (emcp--server-build)
    (let* (requests
           (client (emcp-test-client-init-session
                    server
                    :capabilities '((roots . ((listChanged . t))))
                    :on-request (lambda (r) (push r requests))))
           (session (emcp-test-client-session client))
           (request (car requests)))
      (should (equal (alist-get 'method request) "roots/list"))
      (emcp--server-on-result
       server session
       (emcp-test-to-hash-table `((jsonrpc . "2.0")
                                  (id . ,(alist-get 'id request))
                                  (result . ((roots . [((uri . "file:///path/to/emcp")
                                                        (name . "emcp"))]))))))
      (let ((roots (plist-get session :roots)))
        (should (= (length roots) 1))
        (should (equal (plist-get (car roots) :name) "emcp"))
        (should (equal (plist-get (car roots) :path) "/path/to/emcp"))))))

(ert-deftest emcp-tests-fetch-roots-on-list-changed ()
  ;; "notifications/roots/list_changed" clears the cached roots, triggers a fresh
  ;; "roots/list", and the new result replaces them.
  (emcp-test-with-server server (emcp--server-build)
    (let* (requests
           (client (emcp-test-client-init-session
                    server
                    :capabilities '((roots . ((listChanged . t))))
                    :on-request (lambda (r) (push r requests))))
           (session (emcp-test-client-session client)))
      ;; Resolve the initial fetch with one root.
      (let ((req (pop requests)))
        (emcp--server-on-result
         server session
         (emcp-test-to-hash-table `((jsonrpc . "2.0")
                                    (id . ,(alist-get 'id req))
                                    (result . ((roots . [((uri . "file:///old")
                                                          (name . "old"))])))))))
      ;; Now signal that the list changed; a new request should be sent.
      (emcp--server-on-notification
       server session
       (emcp-test-to-hash-table '((method . "notifications/roots/list_changed"))))
      (let ((req (pop requests)))
        (should (equal (alist-get 'method req) "roots/list"))
        (emcp--server-on-result
         server session
         (emcp-test-to-hash-table `((jsonrpc . "2.0")
                                    (id . ,(alist-get 'id req))
                                    (result . ((roots . [((uri . "file:///new")
                                                          (name . "new"))])))))))
      (let ((roots (plist-get session :roots)))
        (should (= (length roots) 1))
        (should (equal (plist-get (car roots) :name) "new"))))))

(provide 'emcp-core-tests)
;;; emcp-core-tests.el ends here
