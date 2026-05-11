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
(require 'emcp-confirm)
(require 'emcp-tools-eval)
(require 'emcp-tools-send-keys)
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

(defun emcp-tests--emcp-source-files ()
  "Return the paths of some EMCP source files."
  (mapcar #'find-library-name
          '("emcp" "emcp-core" "emcp-confirm" "emcp-http" "emcp-prompts")))

(defmacro emcp-tests-with-emcp-refs-scope (&rest body)
  "Run BODY with `elisp-refs--loaded-paths' restricted to a subset of EMCP.

Scanning every file in `load-history' makes `find-references' tests
slow.  Restricting the search to emcp's files brings runtime down from
seconds to milliseconds while still exercising the same code paths."
  (declare (indent 0) (debug (body)))
  `(cl-letf (((symbol-function 'elisp-refs--loaded-paths)
              #'emcp-tests--emcp-source-files))
     ,@body))

(ert-deftest emcp-tests-find-references-function ()
  ;; "function" kind matches call sites and skips the symbol's own `defun'
  ;; definition.
  (emcp-tests-with-emcp-refs-scope
    (emcp-tests-with-tool-response response 'emcp-tools-find-references
                                   '((symbol . "emcp--server-stop") (kind . "function"))
      (let* ((result (alist-get 'result response))
             (text (alist-get 'text (aref (alist-get 'content result) 0))))
        (should-not (alist-get 'isError result))
        (should (string-match-p "(emcp--server-stop " text))
        (should-not (string-match-p "(defun emcp--server-stop" text))))))

(ert-deftest emcp-tests-find-references-any ()
  ;; "any" kind matches every occurrence regardless of syntactic position, so
  ;; it includes the `defun' definition line that "function" skips.
  (emcp-tests-with-emcp-refs-scope
    (emcp-tests-with-tool-response response 'emcp-tools-find-references
                                   '((symbol . "emcp--server-stop"))
      (let* ((result (alist-get 'result response))
             (text (alist-get 'text (aref (alist-get 'content result) 0))))
        (should-not (alist-get 'isError result))
        (should (string-match-p "(defun emcp--server-stop" text))))))

(ert-deftest emcp-tests-find-references-max-lines ()
  ;; emcp--server-build is called with a multi-line argument list in emcp.el,
  ;; so the un-capped snippet spans several lines.  With the limit set to 1,
  ;; only the first line is kept and a continuation marker is appended after
  ;; the first line of the matched form.
  (let ((emcp-tools-find-references-max-lines 1))
    (emcp-tests-with-emcp-refs-scope
      (emcp-tests-with-tool-response response 'emcp-tools-find-references
                                     '((symbol . "emcp--server-build") (kind . "function"))
        (let* ((result (alist-get 'result response))
               (text (alist-get 'text (aref (alist-get 'content result) 0))))
          (should-not (alist-get 'isError result))
          (should (string-match-p
                   "(emcp--server-build :name[^\n]*\n\\.\\.\\."
                   text)))))))

(ert-deftest emcp-tests-find-references-not-found ()
  (emcp-tests-with-emcp-refs-scope
    (emcp-tests-with-tool-response response 'emcp-tools-find-references
                                   '((symbol . "emcp--this-does-not-exist-at-all"))
      (let* ((result (alist-get 'result response))
             (text (alist-get 'text (aref (alist-get 'content result) 0))))
        (should (eq (alist-get 'isError result) t))
        (should (string-match-p "No symbol named" text))))))

(ert-deftest emcp-tests-find-references-no-matches ()
  ;; A bound symbol that nothing references.
  (let ((sym (make-symbol "emcp-tests--orphan-but-interned")))
    (intern (symbol-name sym))
    (emcp-tests-with-emcp-refs-scope
      (emcp-tests-with-tool-response response 'emcp-tools-find-references
                                     `((symbol . ,(symbol-name sym)))
        (let* ((result (alist-get 'result response))
               (text (alist-get 'text (aref (alist-get 'content result) 0))))
          (should-not (alist-get 'isError result))
          (should (string-match-p "No references to" text)))))))

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

(defvar emcp-tests--var-target nil
  "Scratch variable used by the get/set-variable tool tests.")

(ert-deftest emcp-tests-get-variable ()
  (let ((emcp-tests--var-target '(1 "two" 3)))
    (emcp-tests-with-tool-response response 'emcp-tools-get-variable
                                   '((name . "emcp-tests--var-target"))
      (let ((text (alist-get 'text (aref (alist-get 'content (alist-get 'result response)) 0))))
        (should (equal text "(1 \"two\" 3)"))))))

(ert-deftest emcp-tests-get-variable-unbound ()
  (emcp-tests-with-tool-response response 'emcp-tools-get-variable
                                 '((name . "emcp-tests--definitely-not-a-variable"))
    (let* ((result (alist-get 'result response))
           (text (alist-get 'text (aref (alist-get 'content result) 0))))
      (should (eq (alist-get 'isError result) t))
      (should (string-match-p "not bound" text)))))

(ert-deftest emcp-tests-set-variable-literal ()
  (let ((emcp-tests--var-target nil))
    (emcp-tests-with-tool-response response 'emcp-tools-set-variable
                                   '((name . "emcp-tests--var-target")
                                     (value . "(1 2 3)"))
      (should-not (alist-get 'isError (alist-get 'result response)))
      ;; `(1 2 3)' is stored as data, not evaluated
      (should (equal emcp-tests--var-target '(1 2 3))))))

(ert-deftest emcp-tests-set-variable-trailing-junk ()
  (let ((emcp-tests--var-target 'before))
    (emcp-tests-with-tool-response response 'emcp-tools-set-variable
                                   '((name . "emcp-tests--var-target")
                                     (value . "42 extra"))
      (let* ((result (alist-get 'result response))
             (text (alist-get 'text (aref (alist-get 'content result) 0))))
        (should (eq (alist-get 'isError result) t))
        (should (string-match-p "Trailing" text))
        ;; The value must not have been set
        (should (eq emcp-tests--var-target 'before))))))

(ert-deftest emcp-tests-set-variable-malformed ()
  (let ((emcp-tests--var-target 'before))
    (emcp-tests-with-tool-response response 'emcp-tools-set-variable
                                   '((name . "emcp-tests--var-target")
                                     (value . "(unbalanced"))
      (let ((result (alist-get 'result response)))
        (should (eq (alist-get 'isError result) t))
        (should (eq emcp-tests--var-target 'before))))))

(ert-deftest emcp-tests-set-variable-unbound ()
  (emcp-tests-with-tool-response response 'emcp-tools-set-variable
                                 '((name . "emcp-tests--never-defined-var")
                                   (value . "42"))
    (let ((result (alist-get 'result response)))
      (should (eq (alist-get 'isError result) t))
      ;; And the symbol must not have become bound as a side effect
      (should-not (boundp 'emcp-tests--never-defined-var)))))

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

;;; Eval tool

(ert-deftest emcp-tests-eval-parse-single-form ()
  (should (equal (emcp-tools-eval--parse "(+ 1 2)")
                 '(+ 1 2))))

(ert-deftest emcp-tests-eval-parse-multiple-forms ()
  ;; Multiple top-level forms are wrapped in `progn'.
  (should (equal (emcp-tools-eval--parse "(setq x 1) (setq y 2)")
                 '(progn (setq x 1) (setq y 2)))))

(ert-deftest emcp-tests-eval-parse-malformed ()
  (should-error (emcp-tools-eval--parse "(unbalanced")
                :type 'emcp-tools-eval-parse-error))

(ert-deftest emcp-tests-eval-parse-empty ()
  (should-error (emcp-tools-eval--parse "   \n  ")
                :type 'emcp-tools-eval-parse-error))

(ert-deftest emcp-tests-eval-authorize-session-mode-wins ()
  ;; Session mode `reject' overrides a persistent `t' for the same form.
  (let* ((session `(:emcp-tools-eval-mode reject))
         (persistent (list (cons '(safe-form) t))))
    (should (eq (emcp-tools-eval--authorize '(safe-form) session persistent) nil))))

(ert-deftest emcp-tests-eval-authorize-session-cache-then-persistent ()
  (let* ((session `(:emcp-tools-eval-cache (((+ 1 2) . t))))
         (persistent (list (cons '(+ 1 2) nil))))
    ;; Session cache wins over persistent.
    (should (eq (emcp-tools-eval--authorize '(+ 1 2) session persistent) t))))

(ert-deftest emcp-tests-eval-authorize-persistent-reject ()
  ;; A persistent nil entry must be honored as a rejection, not treated as a miss.
  (let ((emcp-tools-eval-default-policy t))
    (should (eq (emcp-tools-eval--authorize '(bad-form) nil
                                            (list (cons '(bad-form) nil)))
                nil))))

(ert-deftest emcp-tests-eval-authorize-default-action ()
  (let ((emcp-tools-eval-default-policy t))
    (should (eq (emcp-tools-eval--authorize '(any) nil nil) t)))
  (let ((emcp-tools-eval-default-policy nil))
    (should (eq (emcp-tools-eval--authorize '(any) nil nil) nil)))
  (let ((emcp-tools-eval-default-policy 'query))
    (should (eq (emcp-tools-eval--authorize '(any) nil nil) 'prompt))))

(ert-deftest emcp-tests-eval-action-yes-session ()
  ;; Calling the action handler updates the session cache and returns t.
  (let ((session (list :emcp-tools-eval-cache nil)))
    (should (eq (emcp-tools-eval--apply-action 'yes-session session '(+ 1 2))
                t))
    (should (equal (plist-get session :emcp-tools-eval-cache)
                   '(((+ 1 2) . t))))))

(ert-deftest emcp-tests-eval-action-no-always-persists ()
  ;; The `no-always' action must persist a nil (reject) decision to disk
  ;; and a fresh read must recover it as nil, not as a miss.  The
  ;; `'missing' default to `alist-get' is what distinguishes the two.
  (let* ((tmp (make-temp-file "emcp-forms" nil ".eld"))
         (emcp-tools-eval-cache-file tmp)
         (emcp-tools-eval--decisions-cache nil)
         (session (list)))
    (unwind-protect
        (progn
          (should (eq (emcp-tools-eval--apply-action 'no-always
                                                     session '(rm-rf "/"))
                      nil))
          (setq emcp-tools-eval--decisions-cache nil)
          (should (equal (alist-get '(rm-rf "/")
                                    (emcp-tools-eval--recorded-decisions)
                                    'missing nil #'equal)
                         nil)))
      (delete-file tmp))))

(ert-deftest emcp-tests-eval-action-mode-accept ()
  ;; Real sessions are created by `emcp--server-on-initialize' with at least
  ;; an :id key, so `plist-put' mutates in place.  Use a session with one key
  ;; here so the same is true.
  (let ((session (list :id "test-session")))
    (should (eq (emcp-tools-eval--apply-action 'mode-accept session '(form))
                t))
    (should (eq (plist-get session :emcp-tools-eval-mode) 'accept))))

(ert-deftest emcp-tests-eval-tool-default-accept ()
  (let ((emcp-tools-eval-default-policy t))
    (emcp-tests-with-tool-response response 'emcp-tools-eval
                                   '((code . "(+ 40 2)"))
      (let ((text (alist-get 'text (aref (alist-get 'content
                                                    (alist-get 'result response)) 0))))
        (should (equal text "42"))))))

(ert-deftest emcp-tests-eval-tool-default-reject ()
  (let ((emcp-tools-eval-default-policy nil))
    (emcp-tests-with-tool-response response 'emcp-tools-eval
                                   '((code . "(+ 1 2)"))
      (should (eq (alist-get 'isError (alist-get 'result response)) t)))))

(ert-deftest emcp-tests-eval-tool-malformed ()
  (let ((emcp-tools-eval-default-policy t))
    (emcp-tests-with-tool-response response 'emcp-tools-eval
                                   '((code . "(unbalanced"))
      (let* ((result (alist-get 'result response))
             (text (alist-get 'text (aref (alist-get 'content result) 0))))
        (should (eq (alist-get 'isError result) t))
        (should (string-match-p "Unbalanced\\|incomplete" text))))))

(ert-deftest emcp-tests-eval-tool-runtime-error ()
  (let ((emcp-tools-eval-default-policy t))
    (emcp-tests-with-tool-response response 'emcp-tools-eval
                                   '((code . "(error \"boom\")"))
      (let* ((result (alist-get 'result response)))
        (should (eq (alist-get 'isError result) t))
        (let ((text (alist-get 'text (aref (alist-get 'content result) 0))))
          (should (string-match-p "boom" text)))))))

(defvar emcp-tests--probe nil
  "Scratch variable used by the eval tool's `progn'-wrap test.")

(ert-deftest emcp-tests-eval-tool-progn-wrap ()
  ;; Multiple top-level forms: result is the value of the last.
  (let ((emcp-tools-eval-default-policy t)
        (emcp-tests--probe nil))
    (emcp-tests-with-tool-response response 'emcp-tools-eval
                                   '((code . "(setq emcp-tests--probe 1) (1+ emcp-tests--probe)"))
      (let ((text (alist-get 'text (aref (alist-get 'content
                                                    (alist-get 'result response)) 0))))
        (should (equal text "2"))
        (should (= emcp-tests--probe 1))))))

;;; Send-keys tool

(ert-deftest emcp-tests-send-keys-authorize-mode-accept ()
  ;; Session mode `accept' overrides a default-reject policy.
  (let ((emcp-tools-send-keys-default-policy nil))
    (should (eq (emcp-tools-send-keys--authorize
                 (list :emcp-tools-send-keys-mode 'accept))
                t))))

(ert-deftest emcp-tests-send-keys-authorize-mode-reject ()
  ;; Session mode `reject' overrides a default-accept policy.
  (let ((emcp-tools-send-keys-default-policy t))
    (should (eq (emcp-tools-send-keys--authorize
                 (list :emcp-tools-send-keys-mode 'reject))
                nil))))

(ert-deftest emcp-tests-send-keys-authorize-default ()
  (let ((emcp-tools-send-keys-default-policy t))
    (should (eq (emcp-tools-send-keys--authorize nil) t)))
  (let ((emcp-tools-send-keys-default-policy nil))
    (should (eq (emcp-tools-send-keys--authorize nil) nil)))
  (let ((emcp-tools-send-keys-default-policy 'query))
    (should (eq (emcp-tools-send-keys--authorize nil) 'prompt))))

(ert-deftest emcp-tests-send-keys-action-once ()
  (should (eq (emcp-tools-send-keys--apply-action 'yes-once nil) t))
  (should (eq (emcp-tools-send-keys--apply-action 'no-once nil) nil)))

(ert-deftest emcp-tests-send-keys-action-mode ()
  ;; Real sessions always have at least an :id key so `plist-put' mutates
  ;; in place.
  (let ((session (list :id "test-session")))
    (should (eq (emcp-tools-send-keys--apply-action 'mode-accept session) t))
    (should (eq (plist-get session :emcp-tools-send-keys-mode) 'accept))
    (should (eq (emcp-tools-send-keys--apply-action 'mode-reject session) nil))
    (should (eq (plist-get session :emcp-tools-send-keys-mode) 'reject))))

(ert-deftest emcp-tests-send-keys-tool-default-accept ()
  ;; Self-insert keys land in the buffer of the selected window at call time.
  (let ((emcp-tools-send-keys-default-policy t)
        (buf (generate-new-buffer " *emcp-tests-send-keys-accept*")))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buf)
          (emcp-tests-with-tool-response response 'emcp-tools-send-keys
                                         '((keys . "h e l l o"))
            (should-not (alist-get 'isError
                                   (alist-get 'result response))))
          (with-current-buffer buf
            (should (equal (buffer-string) "hello"))))
      (kill-buffer buf))))

(ert-deftest emcp-tests-send-keys-tool-default-reject ()
  (let ((emcp-tools-send-keys-default-policy nil)
        (buf (generate-new-buffer " *emcp-tests-send-keys-reject*")))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buf)
          (emcp-tests-with-tool-response response 'emcp-tools-send-keys
                                         '((keys . "h e l l o"))
            (should (eq (alist-get 'isError
                                   (alist-get 'result response))
                        t)))
          ;; Buffer must remain untouched on rejection
          (with-current-buffer buf
            (should (equal (buffer-string) ""))))
      (kill-buffer buf))))

(ert-deftest emcp-tests-send-keys-execute-window-dead ()
  ;; A dead target window yields an error response without executing keys.
  (let ((response nil)
        (win (split-window)))
    (delete-window win)
    (emcp-tools-send-keys--execute "h e l l o" win
                                   (lambda (r) (setq response r)))
    (should (eq (alist-get 'isError response) t))))

;;; Confirm buffer

(ert-deftest emcp-tests-confirm-result-action ()
  ;; Pressing a :result key invokes the callback once with that symbol and
  ;; kills the buffer.  Setting :on-dismiss also exercises the dispatch
  ;; path's `--pending'-clearing: if the kill-buffer-hook fired again, the
  ;; callback would be invoked twice.
  (let* ((calls nil)
         (cb (lambda (r) (push r calls))))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (let ((buf (emcp-confirm-prompt
                  :session '(:id "test-session")
                  :title "do thing"
                  :body "thing"
                  :on-dismiss 'no-once
                  :groups '((:title "Accept?"
                                    :actions ((?y "Yes" :result yes-once)
                                              (?n "No"  :result no-once))))
                  :callback cb)))
        (with-current-buffer buf (emcp-confirm--dispatch 'yes-once))
        (should (equal calls '(yes-once)))
        (should-not (buffer-live-p buf))))))

(ert-deftest emcp-tests-confirm-command-action-does-not-dismiss ()
  ;; A :command action runs the bound function without dismissing the buffer
  ;; or invoking the callback.
  (let* ((calls nil)
         (cmd-runs 0)
         (cmd (lambda () (interactive) (cl-incf cmd-runs)))
         (cb (lambda (r) (push r calls))))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (let ((buf (emcp-confirm-prompt
                  :session '(:id "test")
                  :title "do thing"
                  :body "thing"
                  :groups `((:actions ((?w "Aux" :command ,cmd))))
                  :callback cb)))
        (with-current-buffer buf
          (call-interactively (lookup-key (current-local-map) "w")))
        (should (= cmd-runs 1))
        (should (null calls))
        (should (buffer-live-p buf))
        (kill-buffer buf)))))

(ert-deftest emcp-tests-confirm-dismiss-invokes-callback ()
  ;; Killing the buffer without picking a :result action invokes the
  ;; callback with the :on-dismiss symbol.
  (let* ((calls nil)
         (cb (lambda (r) (push r calls))))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (let ((buf (emcp-confirm-prompt
                  :session '(:id "test")
                  :title "do thing"
                  :body ""
                  :on-dismiss 'no-once
                  :groups '((:actions ((?y "Yes" :result yes-once))))
                  :callback cb)))
        (kill-buffer buf)
        (should (equal calls '(no-once)))))))

(provide 'emcp-tests)
;;; emcp-tests.el ends here
