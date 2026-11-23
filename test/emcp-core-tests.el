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
      (let ((client (emcp-test-client-init-session
                     server :client-info '((name . "test-client") (version . "1.0")))))
        (emcp-test-client-with-response client response
                                        `((id . 42)
                                          (method . "tools/call")
                                          (params . ((name . ,(symbol-name tool)))))
          (should (equal (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))
                         "test-client 2025-11-25 42 emcp-test")))
        (emcp-test-client-with-response client response
                                        `((id . 43)
                                          (method . "prompts/get")
                                          (params . ((name . ,(symbol-name prompt)))))
          (let ((message (aref (alist-get 'messages (alist-get 'result response)) 0)))
            (should (equal (alist-get 'text (alist-get 'content message))
                           "test-client 2025-11-25 43 emcp-test"))))
        (emcp-test-client-with-response client response
                                        '((id . 44)
                                          (method . "resources/read")
                                          (params . ((uri . "emcp://test/context"))))
          (should (equal (alist-get 'text (aref (alist-get 'contents (alist-get 'result response)) 0))
                         "test-client 2025-11-25 44 emcp-test")))))))

(ert-deftest emcp-tests-log-prefix-client-label ()
  ;; Log lines start with the client name, truncated or padded to a fixed width.
  (emcp-test-with-server server (emcp--server-build)
    (dolist (name '("a-very-long-client-name" "short"))
      (let ((client (emcp-test-client-init-session server :client-info `((name . ,name)))))
        (emcp-test-request client "tools/list")))
    (with-current-buffer (emcp--server-log-buffer server)
      (let ((log (buffer-string)))
        (should (string-match-p "^[^ ]+ [^ ]+ INFO  a-very-lon > Request tools/list" log))
        (should (string-match-p "^[^ ]+ [^ ]+ INFO  short      > Request tools/list" log))))))

;;; Server notifications

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

(provide 'emcp-core-tests)
;;; emcp-core-tests.el ends here
