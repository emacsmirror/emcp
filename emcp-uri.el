;;; emcp-uri.el --- URI template utilities for EMCP -*- lexical-binding: t -*-

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

;; A small subset of RFC 6570 URI templates: compile a template like
;; "info://{manual}/{node}" into a regex, match a URI against the regex to extract
;; parameters, or build a URI by substituting parameters back into a template.

;;; Code:

(require 'cl-lib)
(require 'url-util)

(defun emcp-uri--extract-params (template)
  "Extract parameter symbols from URI TEMPLATE.

Return a list of symbols for each {param} placeholder."
  (let (params (pos 0))
    (while (string-match "{\\([^}]+\\)}" template pos)
      (push (intern (match-string 1 template)) params)
      (setq pos (match-end 0)))
    (nreverse params)))

(defun emcp-uri--compile-template (template)
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

(defun emcp-uri--match (uri compiled-template)
  "Match URI against COMPILED-TEMPLATE.

COMPILED-TEMPLATE is (REGEX . PARAMS) as returned by
`emcp-uri--compile-template'.  Return an alist of (PARAM . DECODED-VALUE)
or nil if URI does not match."
  (pcase-let ((`(,regex . ,params) compiled-template))
    (when (string-match regex uri)
      (cl-loop for param in params
               for i from 1
               collect (cons param (url-unhex-string (match-string i uri)))))))

(defun emcp-uri--build (template params)
  "Build a URI from TEMPLATE by substituting PARAMS.

PARAMS is an alist of (PARAM . VALUE).  Values are percent-encoded."
  (let ((uri template))
    (pcase-dolist (`(,param . ,value) params)
      (setq uri (replace-regexp-in-string
                 (regexp-quote (concat "{" (symbol-name param) "}"))
                 (url-hexify-string value)
                 uri t t)))
    uri))

(provide 'emcp-uri)
;;; emcp-uri.el ends here
