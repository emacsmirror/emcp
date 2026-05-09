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

(require 'emcp-core)

(declare-function x-export-frames "xfns.c")

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
