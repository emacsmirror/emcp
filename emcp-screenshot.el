;;; emcp-screenshot.el --- Frame screenshot capabilities for EMCP -*- lexical-binding: t -*-

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

;; Capture Emacs frames as PNG bytes and expose them via the
;; `emcp-tools-screenshot' tool and the `emcp-prompt-screenshot' prompt.
;;
;; `x-export-frames' only exists in builds with X11 support, so on `--with-ns'
;; Emacs (and any other build that lacks it) we fall back to macOS
;; `screencapture -R' against each frame's screen rectangle.

;;; Code:

(require 'cl-lib)
(require 'seq)

(require 'emcp-core)

(declare-function x-export-frames "xfns.c")

(defun emcp-screenshot--screencapture-frame (frame)
  "Return PNG bytes for FRAME via macOS `screencapture', or nil on failure.

Captures the screen rectangle occupied by FRAME, so any window occluding
the frame will appear in the screenshot.  Requires Screen Recording
permission for the Emacs binary."
  (when-let* ((capture (executable-find "screencapture"))
              (pos (frame-position frame))
              (left (car pos))
              (top (cdr pos))
              (width (frame-pixel-width frame))
              (height (frame-pixel-height frame))
              (tmp (make-temp-file "emcp-screenshot-" nil ".png")))
    (unwind-protect
        (when (and (eq 0 (call-process capture nil nil nil
                                       "-x" "-t" "png"
                                       "-R" (format "%d,%d,%d,%d"
                                                    left top width height)
                                       tmp))
                   (> (file-attribute-size (file-attributes tmp)) 0))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally tmp)
            (buffer-string)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(defun emcp-screenshot-frame-png (frame)
  "Return PNG bytes capturing FRAME, or nil if no backend is available."
  (cond
   ((fboundp 'x-export-frames) (x-export-frames frame 'png))
   ((eq (framep frame) 'ns) (emcp-screenshot--screencapture-frame frame))))

(defun emcp-screenshot-available-p ()
  "Return non-nil when at least one screenshot backend is usable."
  (and (display-graphic-p)
       (or (fboundp 'x-export-frames)
           (and (eq (framep (selected-frame)) 'ns)
                (executable-find "screencapture")))))

(emcp-deftool emcp-tools-screenshot ()
  "View screenshots of all visible Emacs frames."
  :name "screenshot"
  (if (emcp-screenshot-available-p)
      (let* ((frames (seq-filter #'frame-visible-p (frame-list)))
             (blocks (cl-loop for frame in frames
                              for png = (emcp-screenshot-frame-png frame)
                              when png
                              collect `((type . "image")
                                        (data . ,(base64-encode-string png t))
                                        (mimeType . "image/png")))))
        (if blocks
            `((content . ,(vconcat blocks)))
          '((content . [((type . "text")
                         (text . "No visible frame could be captured."))])
            (isError . t))))
    '((content . [((type . "text")
                   (text . "Cannot take screenshots in this Emacs build"))])
      (isError . t))))

(emcp-defprompt emcp-prompt-screenshot ()
  "Share screenshots of all visible Emacs frames."
  :name "screenshot"
  (let* ((frames (seq-filter #'frame-visible-p (frame-list)))
         (available-p (emcp-screenshot-available-p))
         (captures (when available-p
                     (cl-loop for frame in frames
                              for png = (emcp-screenshot-frame-png frame)
                              when png collect png)))
         (n (length captures))
         (messages
          (list `((role . "user")
                  (content . ((type . "text")
                              (text . ,(if available-p
                                           (format "This is what I see in Emacs: (%d %s)"
                                                   n (if (= n 1) "frame" "frames"))
                                         "Cannot take screenshot in this Emacs build"))))))))
    (cl-loop for png in captures
             for i from 1
             do (when (> n 1)
                  (push `((role . "user")
                          (content . ((type . "text")
                                      (text . ,(format "Frame %d" i)))))
                        messages))
             (push `((role . "user")
                     (content . ((type . "image")
                                 (data . ,(base64-encode-string png t))
                                 (mimeType . "image/png"))))
                   messages))
    `((messages . ,(vconcat (nreverse messages))))))

(provide 'emcp-screenshot)
;;; emcp-screenshot.el ends here
