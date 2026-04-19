;;; emcp-resources.el --- Resource definitions for EMCP -*- lexical-binding: t -*-

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

;; MCP resource templates as defined by the spec [1].
;;
;; [1] https://modelcontextprotocol.io/specification/2025-11-25/server/resources

;;; Code:

(require 'cl-lib)
(require 'info)
(require 'url-util)

;;; URI template utilities
;; Simple subset of RFC 6570 handling {param} in path segments.

(defun emcp-resources--compile-uri-template (template)
  "Compile URI TEMPLATE into a regex and parameter list.

Return (REGEX . PARAMS) where REGEX is a string that matches URIs
conforming to TEMPLATE and PARAMS is the ordered list of parameter
symbols corresponding to capture groups in REGEX."
  (let (params
        (pos 0)
        (regex "\\`"))
    (while (string-match "{\\([^}]+\\)}" template pos)
      (setq regex (concat regex
                          (regexp-quote (substring template pos (match-beginning 0)))
                          "\\([^/]+\\)"))
      (push (intern (match-string 1 template)) params)
      (setq pos (match-end 0)))
    (setq regex (concat regex (regexp-quote (substring template pos)) "\\'"))
    (cons regex (nreverse params))))

(defun emcp-resources--match-uri (uri compiled-template)
  "Match URI against COMPILED-TEMPLATE.

COMPILED-TEMPLATE is (REGEX . PARAMS) as returned by
`emcp-resources--compile-uri-template'.  Return an alist of (PARAM
. DECODED-VALUE) or nil if URI does not match."
  (pcase-let ((`(,regex . ,params) compiled-template))
    (when (string-match regex uri)
      (cl-loop for param in params
               for i from 1
               collect (cons param (url-unhex-string (match-string i uri)))))))

(defun emcp-resources--build-uri (template params)
  "Build a URI from TEMPLATE by substituting PARAMS.

PARAMS is an alist of (PARAM . VALUE).  Values are percent-encoded."
  (let ((uri template))
    (pcase-dolist (`(,param . ,value) params)
      (setq uri (replace-regexp-in-string
                 (regexp-quote (concat "{" (symbol-name param) "}"))
                 (url-hexify-string value)
                 uri t t)))
    uri))

;;; Resource macro

(eval-and-compile
  (defun emcp-resources--extract-params (uri-or-template)
    "Extract parameter symbols from URI-OR-TEMPLATE.

Return a list of symbols for each {param} placeholder."
    (let (params (pos 0))
      (while (string-match "{\\([^}]+\\)}" uri-or-template pos)
        (push (intern (match-string 1 uri-or-template)) params)
        (setq pos (match-end 0)))
      (nreverse params))))

(defmacro emcp-defresource (name uri-or-template docstring &rest body)
  "Define NAME as an MCP resource or resource template.

URI-OR-TEMPLATE is a URI string, optionally containing {param}
placeholders per RFC 6570.  Parameter symbols are extracted
automatically and bound in BODY.  If there are no placeholders, NAME is
a static resource listed via resources/list; otherwise it is a resource
template listed via resources/templates/list.

The following keyword options may appear before BODY:

 :name MCP resource name (default NAME).
 :title MCP resource title.
 :description MCP resource description (default DOCSTRING).
 :mime-type MIME type of the resource content.
 :async When non-nil, BODY handles responses manually via locally bound
        functions `send-result' and `send-error'.  When nil, BODY
        returns a resource result directly.

When the client reads the resource, execute BODY to produce a result.
In addition to the template parameters, the following symbols are bound
in BODY:

 `server': The MCP server.
 `session': Client's MCP session.
 `uri': The full request URI.

If :async is nil or not provided, BODY just returns a resources/read
result alist as described in the MCP specification.  If :async is
non-nil, the following functions are bound and BODY has to call exactly
one of them once to send a result or error:

 `send-result' (RESULT): Send the resource RESULT to the client.
 `send-error' (CODE MESSAGE &optional DATA): Signal an error to the
 client with error code CODE, MESSAGE and optional error DATA.

RESULT is a resources/read result JSON document (an alist) as described
in the spec, see this URL
https://modelcontextprotocol.io/specification/2025-11-25/server/resources"
  (declare (indent 2) (debug (symbolp stringp stringp body)))
  (let (mcp-name mcp-title mcp-description mime-type async-p)
    (while (keywordp (car body))
      (pcase (pop body)
        (:name (setq mcp-name (pop body)))
        (:title (setq mcp-title (pop body)))
        (:description (setq mcp-description (pop body)))
        (:mime-type (setq mime-type (pop body)))
        (:async (setq async-p (pop body)))))
    (unless mcp-name
      (setq mcp-name (symbol-name name)))
    (unless mcp-description
      (setq mcp-description docstring))
    (let* ((args (emcp-resources--extract-params uri-or-template))
           (params-var (gensym "params"))
           (arg-bindings
            (cl-loop for sym in args
                     collect `(,sym (alist-get ',sym ,params-var))))
           (send-result-var (gensym "send-result"))
           (send-error-var (gensym "send-error")))
      (let ((metadata
             `((,(if args 'uriTemplate 'uri) . ,uri-or-template)
               (name . ,mcp-name)
               ,@(when mcp-title `((title . ,mcp-title)))
               (description . ,mcp-description)
               ,@(when mime-type `((mimeType . ,mime-type))))))
        (if args
            ;; Resource template
            `(progn
               (put ',name 'emcp-resource-template
                    (list :name ,uri-or-template
                          :metadata ',metadata
                          :match (emcp-resources--compile-uri-template
                                  ,uri-or-template)))
               (defun ,name (server session ,send-result-var ,send-error-var uri ,params-var)
                 ,docstring
                 (ignore server session uri)
                 ,(if async-p
                      `(cl-flet ((send-result (result)
                                   (funcall ,send-result-var result))
                                 (send-error (code message &optional data)
                                   (funcall ,send-error-var code message data)))
                         (let ,arg-bindings
                           ,@body))
                    `(let ,arg-bindings
                       (funcall ,send-result-var (progn ,@body))))))
          ;; Static resource
          `(progn
             (put ',name 'emcp-resource
                  '(:name ,uri-or-template :metadata ,metadata))
             (defun ,name (server session ,send-result-var ,send-error-var uri)
               ,docstring
               (ignore server session uri)
               ,(if async-p
                    `(cl-flet ((send-result (result)
                                 (funcall ,send-result-var result))
                               (send-error (code message &optional data)
                                 (funcall ,send-error-var code message data)))
                       ,@body)
                  `(funcall ,send-result-var (progn ,@body))))))))))

;;; Info manual resource

(defun emcp-resources--info-replace-xrefs (text manual)
  "Replace *note cross-references in TEXT with info:// URIs.

MANUAL is the current manual for same-manual references."
  (replace-regexp-in-string
   "\\*[Nn]ote[ \t\n]+\\([^:]*\\):\\(?::\\|[ \t\n]+\\(?:(\\([^)]+\\))\\)?\\([^.,\t]*\\)[.,]\\)"
   (lambda (match)
     (let* ((raw-label (match-string 1 match))
            (xref-manual (match-string 2 match))
            (node-ref (match-string 3 match)))
       (save-match-data
         (let* ((label (replace-regexp-in-string "[ \t\n]+" " " raw-label))
                (m (or xref-manual manual))
                (n (if (or (null node-ref) (string-empty-p (string-trim node-ref)))
                       label
                     (string-trim node-ref))))
           ;; Parse (manual)Node from label or node reference
           (when (string-match "^(\\([^)]+\\))\\(.*\\)" n)
             (setq m (match-string 1 n))
             (let ((node-part (match-string 2 n)))
               (setq n (if (string-empty-p node-part) "Top" node-part)))
             ;; Clean up label if it contains the (manual) prefix
             (when (string-match "^(\\([^)]+\\))\\(.*\\)" label)
               (setq label (match-string 2 label))
               (when (string-empty-p label) (setq label n))))
           (format "%s (%s)" label
                   (emcp-resources--build-uri
                    "info://{manual}/{node}"
                    `((manual . ,m) (node . ,n))))))))
   text t))

(defun emcp-resources--info-read-node (manual node)
  "Read NODE from MANUAL and return formatted text with navigation URIs."
  (with-temp-buffer
    (Info-mode)
    (Info-find-node manual node)
    (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
           (text (emcp-resources--info-replace-xrefs text manual))
           (nav (emcp-resources--info-extract-nav))
           (menu (emcp-resources--info-extract-menu manual))
           (uris (emcp-resources--info-format-nav-uris manual nav menu)))
      (if uris (concat text "\n\n" uris) text))))

(defun emcp-resources--info-extract-nav ()
  "Extract navigation pointers from the current Info buffer.

Return an alist with keys `next', `prev', `up' where present."
  (save-excursion
    (goto-char (point-min))
    (let ((header (buffer-substring-no-properties
                   (point) (line-end-position)))
          nav)
      (dolist (pointer '(next prev up))
        (let ((name (capitalize (symbol-name pointer))))
          (when (string-match (format "%s: \\([^,\t\n]+\\)" name) header)
            (push (cons pointer (string-trim (match-string 1 header))) nav))))
      (nreverse nav))))

(defun emcp-resources--info-extract-menu (current-manual)
  "Extract menu items from the current Info buffer.

CURRENT-MANUAL is used to construct URIs for same-manual links.
Return a list of (LABEL MANUAL NODE) triples."
  (save-excursion
    (goto-char (point-min))
    (let (items)
      (when (re-search-forward "^\\* Menu:" nil t)
        (while (re-search-forward
                "^\\* +\\([^:\t\n]+\\):\\(:\\|[ \t]+\\([^.,\t\n]+\\)[.,]\\)"
                nil t)
          (let* ((label (string-trim (match-string 1)))
                 (node-ref (if (match-string 3)
                               (string-trim (match-string 3))
                             label)))
            ;; Handle cross-manual references: (manual)Node
            (if (string-match "^(\\([^)]+\\))\\(.*\\)" node-ref)
                (let ((m (match-string 1 node-ref))
                      (n (match-string 2 node-ref)))
                  (push (list label m (if (string-empty-p n) "Top" n)) items))
              (push (list label current-manual node-ref) items)))))
      (nreverse items))))

(defun emcp-resources--info-nav-uri (manual node)
  "Build an info:// URI for NODE in MANUAL.

NODE may contain a cross-manual reference like \"(other)Top\"."
  (if (string-match "^(\\([^)]+\\))\\(.*\\)" node)
      (let ((m (match-string 1 node))
            (n (match-string 2 node)))
        (emcp-resources--build-uri
         "info://{manual}/{node}"
         `((manual . ,m) (node . ,(if (string-empty-p n) "Top" n)))))
    (emcp-resources--build-uri
     "info://{manual}/{node}"
     `((manual . ,manual) (node . ,node)))))

(defun emcp-resources--info-format-nav-uris (manual nav menu)
  "Format navigation URIs for an Info node.

MANUAL is the current manual.  NAV is from
`emcp-resources--info-extract-nav'.  MENU is from
`emcp-resources--info-extract-menu'.  Return a string or nil."
  (let (lines)
    (when nav
      (push "Navigation:" lines)
      (dolist (entry nav)
        (push (format "  %s: %s"
                      (capitalize (symbol-name (car entry)))
                      (emcp-resources--info-nav-uri manual (cdr entry)))
              lines)))
    (when menu
      (when lines (push "" lines))
      (push "Menu:" lines)
      (pcase-dolist (`(,label ,m ,node) menu)
        (push (format "  %s: %s" label
                      (emcp-resources--build-uri
                       "info://{manual}/{node}"
                       `((manual . ,m) (node . ,node))))
              lines)))
    (when lines
      (string-join (nreverse lines) "\n"))))

(emcp-defresource emcp-resource-info-node "info://{manual}/{node}"
  "Read a node from an Emacs Info manual."
  :name "info-node"
  :mime-type "text/plain"
  (let ((text (emcp-resources--info-read-node manual node)))
    `((contents . [((uri . ,uri)
                    (mimeType . "text/plain")
                    (text . ,text))]))))

(provide 'emcp-resources)
;;; emcp-resources.el ends here
