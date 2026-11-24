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
;; Supported are simple expansion {var}, whose value is a single path segment, and
;; reserved expansion {+var}, whose value may contain reserved characters such as /.

;;; Code:

(require 'cl-lib)
(require 'url-util)

(defconst emcp-uri--placeholder-regex "{\\(\\+?\\)\\([^}]+\\)}"
  "Regex for a {var} or {+var} placeholder.

Group 1 is the operator, group 2 the parameter name.")

(defconst emcp-uri--reserved-chars
  ;; Unreserved characters plus gen-delims and sub-delims of RFC 3986
  (append url-unreserved-chars (string-to-list ":/?#[]@!$&'()*+,;="))
  "Characters that {+var} expansion leaves unencoded.")

(defun emcp-uri--extract-params (template)
  "Extract parameter symbols from URI TEMPLATE.

Return a list of symbols for each {param} or {+param} placeholder, without
the operator."
  (cdr (emcp-uri--compile-template template)))

(defun emcp-uri--compile-template (template)
  "Compile URI TEMPLATE into a regex and parameter list.

Return (REGEX . PARAMS) where REGEX is a string that matches URIs
conforming to TEMPLATE and PARAMS is the ordered list of parameter
symbols corresponding to capture groups in REGEX.  A {param} placeholder
matches a single path segment, {+param} also matches across /."
  (let (params
        (pos 0)
        (regex "\\`"))
    (while (string-match emcp-uri--placeholder-regex template pos)
      (setq regex (concat regex
                          (regexp-quote (substring template pos (match-beginning 0)))
                          ;; Lenient like {param}, i.e. accept any character
                          ;; instead of only unreserved, reserved and %
                          (if (equal (match-string 1 template) "+")
                              "\\(.+\\)"
                            "\\([^/]+\\)")))
      (push (intern (match-string 2 template)) params)
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

(defun emcp-uri--reserved-expand (value)
  "Percent-encode VALUE for a {+var} placeholder as in RFC 6570.

Keep unreserved and reserved characters as well as existing
percent-encoded triplets like %20, and encode everything else as UTF-8."
  (let* ((bytes (encode-coding-string value 'utf-8))
         (end (length bytes))
         (i 0)
         parts)
    (while (< i end)
      (let ((byte (aref bytes i)))
        (cond
         ((and (eq byte ?%)
               (<= (+ i 3) end)
               (string-match-p "\\`[[:xdigit:]]\\{2\\}" (substring bytes (1+ i) (+ i 3))))
          (push (substring bytes i (+ i 3)) parts)
          (setq i (+ i 3)))
         ((memq byte emcp-uri--reserved-chars)
          (push (string byte) parts)
          (setq i (1+ i)))
         (t
          (push (format "%%%02X" byte) parts)
          (setq i (1+ i))))))
    (apply #'concat (nreverse parts))))

(defun emcp-uri--build (template params)
  "Build a URI from TEMPLATE by substituting PARAMS.

PARAMS is an alist of (PARAM . VALUE).  Values are percent-encoded, except
for reserved characters such as / in {+param} placeholders.  Placeholders
without a value in PARAMS stay as they are."
  (replace-regexp-in-string
   emcp-uri--placeholder-regex
   (lambda (placeholder)
     (if-let* ((value (alist-get (intern (match-string 2 placeholder)) params)))
         (if (equal (match-string 1 placeholder) "+")
             (emcp-uri--reserved-expand value)
           (url-hexify-string value))
       placeholder))
   template t t))

(provide 'emcp-uri)
;;; emcp-uri.el ends here
