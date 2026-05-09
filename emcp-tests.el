;;; emcp-tests.el --- Test suite for emcp.el -*- lexical-binding: t -*-

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

;;; Code:

(require 'cl-lib)
(require 'emcp)
(require 'ert)

(defmacro with-server (server form &rest body)
  "Bind SERVER to FORM, execute BODY, then stop the server."
  (declare (indent 2) (debug (symbolp sexp body)))
  ;; Log at trace level to exercise logging code during tests
  `(let* ((emcp-log-level 'trace)
          (,server ,form))
     (unwind-protect
         (progn ,@body)
       (emcp--server-stop ,server))))

(defun to-hash-table (object)
  "Convert a JSON OBJECT to hash-table presentation."
  (json-parse-string (json-serialize object)))

(cl-defstruct (emcp-tests-client (:constructor emcp-tests-make-client)
                                 (:copier nil))
  "An MCP client for SERVER for testing.

Drives SERVER directly without any transport."
  server session next-request-id)

(cl-defun emcp-tests-client-init-session (server &key on-request on-notification)
  "Initialize a new client session for SERVER.

Calls (ON-REQUEST REQUEST) when the server sends a request
and (ON-NOTIFICATION NOTIFICATION) when the server sends a notification."
  (pcase-let ((`(,session ,response)
               (emcp--server-on-initialize
                server
                (to-hash-table
                 '((id . 1) (params . ((protocolVersion . "2025-11-25"))))))))
    (emcp--server-on-notification
     server session
     (to-hash-table '((method . "notifications/initialized"))))
    (when (or on-request on-notification)
      (emcp--server-on-client-channel
       server session
       (lambda (request)
         (cond
          ((and on-request (alist-get 'id request))
           (funcall on-request request))
          ((and on-notification (not (alist-get 'id request)))
           (funcall on-notification request))))))
    (emcp-tests-make-client :server server
                            :session session
                            :next-request-id 2)))

(defun emcp-tests-client-send-request (client request callback)
  "Send REQUEST through CLIENT, calling CALLBACK with the response.

Adds a request `id' field to REQUEST automatically, unless already set."
  (unless (alist-get 'id request)
    (setf (alist-get 'id request) (emcp-tests-client-next-request-id client))
    (cl-incf (emcp-tests-client-next-request-id client)))
  (emcp--server-on-request
   (emcp-tests-client-server client)
   (emcp-tests-client-session client)
   (to-hash-table request)
   callback))

(defmacro emcp-tests-client-with-response (client response request &rest body)
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
         (emcp-tests-client-send-request
          ,client ,request
          (lambda (,response)
            ,@body
            (setq ,done t)))
         ,@(when wait
             `((with-timeout (,timeout (error "Request timed out after %s seconds" ,timeout))
                 (while (not ,done)
                   (accept-process-output nil 0.01)))))))))

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

    (with-server server (emcp--server-build :prompts (list sym))
      (let ((client (emcp-tests-client-init-session server)))
        (emcp-tests-client-with-response client response '((method . "prompts/list"))
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

        (emcp-tests-client-with-response client response
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

    (with-server server (emcp--server-build :prompts (list sym))
      (let ((client (emcp-tests-client-init-session server)))
        (emcp-tests-client-with-response client response
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

    (with-server server (emcp--server-build :prompts (list sym))
      (let ((client (emcp-tests-client-init-session server)))
        (emcp-tests-client-with-response client response
                                         `((method . "prompts/get")
                                           (params . ((name . ,(symbol-name sym))
                                                      (arguments . ((name . "Emacs")
                                                                    (lang . "Spanish"))))))
          (should (equal (alist-get 'result response)
                         `((description . ,(format "A pretend greeting in Spanish" lang))
                           (messages . [((role . "user")
                                         (content . ((type . "text")
                                                     (text . "Hello, I am Emacs. Pretend this was in Spanish."))))])))))

        (emcp-tests-client-with-response client response
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

    (with-server server (emcp--server-build :tools (list sym))
      (let ((client (emcp-tests-client-init-session server)))
        (emcp-tests-client-with-response client response '((method . "tools/list"))
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

        (emcp-tests-client-with-response client response
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

    (with-server server (emcp--server-build :tools (list sym))
      (let ((client (emcp-tests-client-init-session server)))
        (emcp-tests-client-with-response client response '((method . "tools/list"))
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

    (with-server server (emcp--server-build :tools (list sym))
      (let ((client (emcp-tests-client-init-session server)))
        (emcp-tests-client-with-response client response
                                         `((method . "tools/call")
                                           (params . ((name . ,(symbol-name sym))
                                                      (arguments . ((name . "Emacs")
                                                                    (a . 10)
                                                                    (b . 5))))))
          (should (equal (alist-get 'result response)
                         '((content . [((type . "text")
                                        (text . "Emacs says 15"))])))))

        (emcp-tests-client-with-response client response
                                         `((method . "tools/call")
                                           (params . ((name . ,(symbol-name sym))
                                                      (arguments . ((name . "Gnu")
                                                                    (a . 10)
                                                                    (b . 5))))))
          (should (equal (alist-get 'error response)
                         `((code . 1001)
                           (message . "Addition is hard")
                           (data . ((name . "Gnu")))))))))))

;;; Tools

(defmacro emcp-tests-with-tool-response (response tool-symbol arguments &rest body)
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
       (with-server ,server (emcp--server-build :tools (list ,tool-symbol))
         (let* ((,client (emcp-tests-client-init-session ,server))
                (,request `((method . "tools/call")
                            (params . ((name . ,,name)
                                       (arguments . ,,arguments))))))
           (emcp-tests-client-with-response ,client ,response ,request
             ,@body))))))

(ert-deftest emcp-tests-apropos-any ()
  (emcp-tests-with-tool-response response 'emcp-tools-apropos
                                 '((pattern . "^emcp-tools-apropos$"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (string-match-p "emcp-tools-apropos (function)" text)))))

(ert-deftest emcp-tests-apropos-command ()
  ;; emcp-tools-apropos is a function but not a command
  (emcp-tests-with-tool-response response 'emcp-tools-apropos
                                 '((pattern . "^emcp-tools-apropos$") (kind . "command"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (equal text "No matching symbols found."))))
  ;; emcp-start is a command
  (emcp-tests-with-tool-response response 'emcp-tools-apropos
                                 '((pattern . "^emcp-start$") (kind . "command"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (equal text "emcp-start")))))

(ert-deftest emcp-tests-apropos-unknown-kind ()
  (emcp-tests-with-tool-response response 'emcp-tools-apropos
                                 '((pattern . "whatever") (kind . "bogus"))
    (should (eq (alist-get 'isError (alist-get 'result response)) t))))

(ert-deftest emcp-tests-find-definition-function ()
  (emcp-tests-with-tool-response response 'emcp-tools-find-definition
                                 '((symbol . "emcp--server-stop"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (string-match-p "emcp-core\\.el" text))
      (should (string-match-p "defun emcp--server-stop" text)))))

(ert-deftest emcp-tests-find-definition-not-found ()
  (emcp-tests-with-tool-response response 'emcp-tools-find-definition
                                 '((symbol . "emcp--this-does-not-exist-at-all"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (string-match-p "No definition found" text)))))

(ert-deftest emcp-tests-describe-function ()
  (emcp-tests-with-tool-response response 'emcp-tools-describe
                                 '((symbol . "emcp--server-stop") (kind . "function"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (string-match-p "Stop SERVER" text)))))

(ert-deftest emcp-tests-describe-any ()
  ;; emcp-log-level is both a variable and a constant
  (emcp-tests-with-tool-response response 'emcp-tools-describe
                                 '((symbol . "emcp-log-level"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (string-match-p "\\[variable\\]" text)))))

(ert-deftest emcp-tests-describe-not-found ()
  (emcp-tests-with-tool-response response 'emcp-tools-describe
                                 '((symbol . "emcp--this-does-not-exist-at-all"))
    (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
      (should (string-match-p "No documentation found" text)))))

;;; Resources

(ert-deftest emcp-tests-uri-template-compile ()
  (pcase-let ((`(,regex . ,params)
               (emcp-uri--compile-template "info://{manual}/{node}")))
    (should (equal params '(manual node)))
    (should (string-match-p regex "info://elisp/Symbols"))
    (should-not (string-match-p regex "http://example.com"))))

(ert-deftest emcp-tests-uri-template-match ()
  (let ((compiled (emcp-uri--compile-template "info://{manual}/{node}")))
    (should (equal (emcp-uri--match "info://elisp/Symbols" compiled)
                   '((manual . "elisp") (node . "Symbols"))))
    (should (equal (emcp-uri--match "info://elisp/Buffer%20List" compiled)
                   '((manual . "elisp") (node . "Buffer List"))))
    (should-not (emcp-uri--match "http://example.com" compiled))))

(ert-deftest emcp-tests-uri-build ()
  (should (equal (emcp-uri--build "info://{manual}/{node}"
                                            '((manual . "elisp") (node . "Buffer List")))
                 "info://elisp/Buffer%20List")))

(ert-deftest emcp-tests-resource-template-list ()
  (with-server server (emcp--server-build :resources '(emcp-resource-info-node))
    (let ((client (emcp-tests-client-init-session server)))
      (emcp-tests-client-with-response client response
                                       '((method . "resources/templates/list"))
        (let* ((result (alist-get 'result response))
               (templates (alist-get 'resourceTemplates result)))
          (should (= (length templates) 1))
          (should (equal (alist-get 'uriTemplate (aref templates 0))
                         "info://{manual}/{node}")))))))

(ert-deftest emcp-tests-resource-read-info ()
  (with-server server (emcp--server-build :resources '(emcp-resource-info-node))
    (let ((client (emcp-tests-client-init-session server)))
      (emcp-tests-client-with-response client response
                                       '((method . "resources/read")
                                         (params . ((uri . "info://elisp/Top"))))
        (let* ((result (alist-get 'result response))
               (contents (alist-get 'contents result))
               (text (alist-get 'text (aref contents 0))))
          (should (string-match-p "Emacs Lisp" text)))))))

(ert-deftest emcp-tests-static-resource ()
  (let ((sym (make-symbol "static-resource")))
    (eval
     `(emcp-defresource ,sym "emcp://test/greeting"
        "A static greeting resource."
        :name "greeting"
        `((contents . [((uri . ,uri)
                        (mimeType . "text/plain")
                        (text . "Hello from EMCP"))]))))

    (with-server server (emcp--server-build :resources (list sym))
      (let ((client (emcp-tests-client-init-session server)))
        ;; Should appear in resources/list, not templates/list
        (emcp-tests-client-with-response client response
                                         '((method . "resources/list"))
          (let ((resources (alist-get 'resources (alist-get 'result response))))
            (should (= (length resources) 1))
            (should (equal (alist-get 'uri (aref resources 0))
                           "emcp://test/greeting"))))

        (emcp-tests-client-with-response client response
                                         '((method . "resources/templates/list"))
          (should (equal (alist-get 'resourceTemplates (alist-get 'result response))
                         [])))

        ;; Should be readable
        (emcp-tests-client-with-response client response
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

    (with-server server (emcp--server-build :resources (list sym))
      (let ((client (emcp-tests-client-init-session server)))
        ;; Successful read
        (emcp-tests-client-with-response client response
                                         '((method . "resources/read")
                                           (params . ((uri . "emcp://test/world"))))
          (let ((text (alist-get 'text (aref (alist-get 'contents
                                                        (alist-get 'result response)) 0))))
            (should (equal text "Hello, world"))))

        ;; Error path
        (emcp-tests-client-with-response client response
                                         '((method . "resources/read")
                                           (params . ((uri . "emcp://test/fail"))))
          (should (alist-get 'error response))
          (should (equal (alist-get 'message (alist-get 'error response))
                         "No greeting for you")))))))

(ert-deftest emcp-tests-info-replace-xrefs ()
  ;; *note Node::
  (should (equal (emcp-resources--info-replace-xrefs "*note Symbols::" "elisp")
                 "Symbols (info://elisp/Symbols)"))
  ;; *note Label: Node.
  (should (equal (emcp-resources--info-replace-xrefs "*note my label: Symbols." "elisp")
                 "my label (info://elisp/Symbols)"))
  ;; *note (manual)Node::
  (should (equal (emcp-resources--info-replace-xrefs "*note (emacs)Buffers::" "elisp")
                 "Buffers (info://emacs/Buffers)"))
  ;; *note Label: (manual)Node.
  (should (equal (emcp-resources--info-replace-xrefs "*note see this: (emacs)Buffers." "elisp")
                 "see this (info://emacs/Buffers)")))

(ert-deftest emcp-tests-info-search ()
  (emcp-tests-with-tool-response response 'emcp-tools-info-search
                                 '((pattern . "defun") (manual . "elisp"))
    (let* ((content (alist-get 'content (alist-get 'result response)))
           (first (aref content 0)))
      (should (equal (alist-get 'type first) "resource_link"))
      (should (string-match-p "^info://" (alist-get 'uri first))))))

;;; Server requests & notifications

(ert-deftest emcp-tests-send-server-notification ()
  (with-server server (emcp--server-build)
    (let* (notification
           (client (emcp-tests-client-init-session
                    server
                    :on-notification (lambda (n) (setq notification n))))
           (session (emcp-tests-client-session client)))
      (emcp--server-send-notification
       server session "test/notification"
       :params '((name . "emcp")))
      (should (equal notification
                     '((jsonrpc . "2.0")
                       (method . "test/notification")
                       (params . ((name . "emcp")))))))))

(ert-deftest emcp-tests-send-server-request ()
  (with-server server (emcp--server-build)
    (let* (request
           (client (emcp-tests-client-init-session
                    server
                    :on-request (lambda (r) (setq request r))))
           (session (emcp-tests-client-session client))
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
      (emcp--server-on-result server session (to-hash-table '((jsonrpc . "2.0")
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
      (emcp--server-on-error server session (to-hash-table '((jsonrpc . "2.0")
                                                             (error . ((code . 128)
                                                                       (message . "miserably")
                                                                       (data . "some data")))
                                                             (id . 2))))
      (should (not result))
      (should (equal error '(128 "miserably" "some data"))))))

(provide 'emcp-tests)
;;; emcp-tests.el ends here
