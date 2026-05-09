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

(require 'emcp-core)
(require 'emcp-uri)

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
                   (emcp-uri--build
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
        (emcp-uri--build
         "info://{manual}/{node}"
         `((manual . ,m) (node . ,(if (string-empty-p n) "Top" n)))))
    (emcp-uri--build
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
                      (emcp-uri--build
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
