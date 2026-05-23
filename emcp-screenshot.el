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
;; `x-export-frames' only exists in builds with X11 support.  On `--with-ns'
;; Emacs (and any other build that lacks it) we fall back to macOS
;; `screencapture'.  Prefer `screencapture -l <CGWindowID>', which captures
;; the window directly and works even when it is occluded or offscreen;
;; only fall back to `-R <rect>' (whatever pixels are at the frame's screen
;; rectangle) when the window ID can't be determined.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)

(require 'emcp-core)

(declare-function x-export-frames "xfns.c")

(defconst emcp-screenshot--ns-window-list-script
  "ObjC.import('CoreGraphics');
ObjC.import('Foundation');
const list = $.CGWindowListCopyWindowInfo(0, 0);
const arr = ObjC.castRefToObject(list);
const out = [];
for (let i = 0; i < arr.count; i++) {
  const w = arr.objectAtIndex(i);
  const owner = w.objectForKey('kCGWindowOwnerName');
  if (owner && ObjC.unwrap(owner) === 'Emacs') {
    const name = w.objectForKey('kCGWindowName');
    const num = w.objectForKey('kCGWindowNumber');
    out.push({title: name ? ObjC.unwrap(name) : '',
              id: num ? num.intValue : 0});
  }
}
JSON.stringify(out);"
  "JXA script returning a JSON array of Emacs NS windows.
Each entry is `{title, id}', where id is a CGWindowID.")

(defun emcp-screenshot--ns-window-ids ()
  "Return an alist of (TITLE . CGWindowID) for Emacs NS windows.
Uses `CGWindowListCopyWindowInfo' via JXA; returns nil on failure."
  (when (executable-find "osascript")
    (with-temp-buffer
      (when (eq 0 (call-process "osascript" nil t nil
                                "-l" "JavaScript"
                                "-e" emcp-screenshot--ns-window-list-script))
        (goto-char (point-min))
        (let* ((json-array-type 'list)
               (json-object-type 'alist)
               (json-key-type 'symbol)
               (entries (ignore-errors (json-read))))
          (mapcar (lambda (w)
                    (cons (alist-get 'title w) (alist-get 'id w)))
                  entries))))))

(defun emcp-screenshot--screencapture-window (window-id)
  "Capture CGWindowID WINDOW-ID via `screencapture -l'.
Returns PNG bytes, or nil on failure."
  (when-let* ((capture (executable-find "screencapture"))
              (tmp (make-temp-file "emcp-screenshot-" nil ".png")))
    (unwind-protect
        (when (and (eq 0 (call-process capture nil nil nil
                                       "-x" "-o" "-t" "png"
                                       (format "-l%d" window-id)
                                       tmp))
                   (> (file-attribute-size (file-attributes tmp)) 0))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally tmp)
            (buffer-string)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(defun emcp-screenshot--screencapture-rect (frame)
  "Capture FRAME's screen rectangle via `screencapture -R'.
Returns PNG bytes, or nil on failure.  Any window occluding FRAME will
appear in the screenshot; prefer `-l' via
`emcp-screenshot--screencapture-window' when a CGWindowID is available."
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

(defun emcp-screenshot--screencapture-frame (frame)
  "Return PNG bytes for FRAME via macOS `screencapture'.
Prefer `screencapture -l <CGWindowID>' so the capture succeeds even when
FRAME is occluded or offscreen; fall back to `-R <rect>' when the window
ID can't be matched."
  (or (when-let* ((ids (emcp-screenshot--ns-window-ids))
                  (wid (cdr (assoc (frame-parameter frame 'name) ids))))
        (emcp-screenshot--screencapture-window wid))
      (emcp-screenshot--screencapture-rect frame)))

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
