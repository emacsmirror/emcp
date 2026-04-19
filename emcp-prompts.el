;;; emcp-prompts.el --- Prompt definitions for EMCP -*- lexical-binding: t -*-

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

;; MCP prompts as defined by the spec [1].
;;
;; [1] https://modelcontextprotocol.io/specification/2025-11-25/server/prompts

;;; Code:

(require 'cl-lib)
(require 'seq)

(defmacro emcp-defprompt (name args docstring &rest body)
  "Define NAME as an MCP prompt.

ARGS is a list of argument specifications.  Each element is either a
bare symbol or a list (SYMBOL [DESCRIPTION] [:default DEFAULT]).
Arguments with a :default are optional in MCP; all others are required.
In the BODY, each argument is bound to its value from the MCP request,
falling back to DEFAULT if provided.

The following keyword options may appear before BODY:

 :name MCP prompt name (default NAME).
 :title MCP prompt title.
 :description MCP prompt description (default DOCSTRING).
 :async When non-nil, BODY handles responses manually via locally bound
	      functions `send-result' and `send-error'.  When nil, BODY
	      returns a prompt directly.

When the client requests the prompt, execute BODY to produce a
prompt.  In addition to the declared ARGS, the following symbols are
bound:

 `server': The MCP server.
 `session': Client's MCP session.

If :async is nil or not provided, BODY just returns a prompt as defined
in the MCP specification.  If :async is non-nil, the following functions
are bound and BODY has to call exactly one of them once to send a prompt
or error:

 `send-result' (PROMPT): Send the generated PROMPT to the client.
 `send-error' (CODE MESSAGE &optional DATA): Signal an error to the
 client with error code CODE, MESSAGE and optional error DATA.

RESULT is a prompt/get result JSON document (an alist) as described in the
spec, see this URL
https://modelcontextprotocol.io/specification/2025-11-25/server/prompts"
  (declare (indent 2) (debug (symbolp sexp stringp body)))
  ;; Parse keyword options before body
  (let (mcp-name mcp-title mcp-description async-p)
    (while (keywordp (car body))
      (pcase (pop body)
        (:name (setq mcp-name (pop body)))
        (:title (setq mcp-title (pop body)))
        (:description (setq mcp-description (pop body)))
        (:async (setq async-p (pop body)))))
    (unless mcp-name
      (setq mcp-name (symbol-name name)))
    (unless mcp-description
      (setq mcp-description docstring))
    (let* ((arg-specs
            (cl-loop for arg in args
                     collect (pcase arg
                               ((pred symbolp)
                                (list arg nil nil))
                               (`(,sym ,(and (pred stringp) desc) . ,plist)
                                (list sym desc plist))
                               (`(,sym . ,plist)
                                (list sym nil plist)))))
           (arg-metadata
            (vconcat
             (cl-loop for (sym desc plist) in arg-specs
                      collect `((name . ,(symbol-name sym))
                                ,@(when desc `((description . ,desc)))
                                (required . ,(if (plist-member plist :default) :false t))))))
           (metadata
            `((name . ,mcp-name)
              ,@(when mcp-title `((title ,mcp-title)))
              (description . ,mcp-description)
              (arguments . ,arg-metadata)))
           (args-var (gensym "args"))
           (arg-bindings
            (cl-loop for (sym _desc plist) in arg-specs
                     collect `(,sym (gethash ,(symbol-name sym) ,args-var
                                             ,(plist-get plist :default))))))
      (let ((send-result-var (gensym "send-result"))
            (send-error-var (gensym "send-error")))
        `(progn
           (put ',name 'emcp-prompt '(:name ,mcp-name :metadata ,metadata))
           (defun ,name (server session ,send-result-var ,send-error-var ,args-var)
             ,docstring
             (ignore server session)
             ,(if async-p
                  `(cl-flet ((send-result (result)
                               (funcall ,send-result-var result))
                             (send-error (code message &optional data)
                               (funcall ,send-error-var code message data)))
                     (let ,arg-bindings
                       ,@body))
                `(let ,arg-bindings
                   (funcall ,send-result-var (progn ,@body))))))))))

(emcp-defprompt emcp-prompt-screenshot ()
  "Share screenshots of all visible Emacs frames."
  :name "screenshot"
  (let* ((frames (seq-filter #'frame-visible-p (frame-list)))
         (n (length frames))
         (graphical-p (and (fboundp 'x-export-frames)
                           (display-graphic-p)))
         (messages
          (list `((role . "user")
                  (content . ((type . "text")
                              (text . ,(format "This is what I see in Emacs: (%d %s)"
                                               n (if (= n 1) "frame" "frames")))))))))
    (cl-loop for frame in frames
             for i from 1
             do (when (> n 1)
                  (push `((role . "user")
                          (content . ((type . "text")
                                      (text . ,(format "Frame %d" i)))))
                        messages))
             (if graphical-p
                 (push `((role . "user")
                         (content . ((type . "image")
                                     (data . ,(base64-encode-string
                                               (x-export-frames frame 'png) t))
                                     (mimeType . "image/png"))))
                       messages)
               (push `((role . "user")
                       (content . ((type . "text")
                                   (text . "Cannot take screenshot in non-graphical Emacs"))))
                     messages)))
    `((messages . ,(vconcat (nreverse messages))))))

(provide 'emcp-prompts)
;;; emcp-prompts.el ends here
