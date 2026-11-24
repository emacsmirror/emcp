;;; emcp-tools-eval-tests.el --- Tests for the eval tool -*- lexical-binding: t -*-

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

;; Tests for the eval tool in emcp-tools-eval.el.

;;; Code:

(require 'emcp-test-helpers)
(require 'emcp-tools-eval)
(require 'ert)

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

(ert-deftest emcp-tests-eval-authorize-mode-accept ()
  ;; Server mode `accept' overrides a default-reject policy.
  (emcp-test-with-server server (emcp--server-build)
    (setf (emcp--server-mode server :emcp-tools-eval-mode) 'accept)
    (let ((emcp-tools-eval-default-policy nil))
      (should (eq (emcp-tools-eval--authorize server) t)))))

(ert-deftest emcp-tests-eval-authorize-mode-reject ()
  ;; Server mode `reject' overrides a default-accept policy.
  (emcp-test-with-server server (emcp--server-build)
    (setf (emcp--server-mode server :emcp-tools-eval-mode) 'reject)
    (let ((emcp-tools-eval-default-policy t))
      (should (eq (emcp-tools-eval--authorize server) nil)))))

(ert-deftest emcp-tests-eval-authorize-default-policy ()
  (emcp-test-with-server server (emcp--server-build)
    (let ((emcp-tools-eval-default-policy t))
      (should (eq (emcp-tools-eval--authorize server) t)))
    (let ((emcp-tools-eval-default-policy nil))
      (should (eq (emcp-tools-eval--authorize server) nil)))
    (let ((emcp-tools-eval-default-policy 'ask))
      (should (eq (emcp-tools-eval--authorize server) 'ask)))))

(ert-deftest emcp-tests-eval-action-once ()
  (emcp-test-with-server server (emcp--server-build)
    (should (eq (emcp-tools-eval--apply-action 'yes-once server) t))
    (should (eq (emcp-tools-eval--apply-action 'no-once server) nil))))

(ert-deftest emcp-tests-eval-action-mode ()
  (emcp-test-with-server server (emcp--server-build)
    (let ((emcp-tools-eval-default-policy 'ask))
      (should (eq (emcp-tools-eval--apply-action 'mode-accept server) t))
      (should (eq (emcp-tools-eval--authorize server) t))
      (should (eq (emcp-tools-eval--apply-action 'mode-reject server) nil))
      (should (eq (emcp-tools-eval--authorize server) nil)))))

(ert-deftest emcp-tests-eval-tool-default-accept ()
  (let ((emcp-tools-eval-default-policy t))
    (emcp-test-with-tool-response response 'emcp-tools-eval
                                  '((code . "(+ 40 2)"))
      (let ((text (alist-get 'text (aref (alist-get 'content
                                                    (alist-get 'result response)) 0))))
        (should (equal text "```emacs-lisp\n42\n```"))))))

(ert-deftest emcp-tests-eval-tool-string-verbatim ()
  ;; A string result is printed verbatim, without quotes or escaping.
  (let ((emcp-tools-eval-default-policy t))
    (emcp-test-with-tool-response response 'emcp-tools-eval
                                  '((code . "(concat \"fo\\\"o\" \"bar\")"))
      (let ((text (alist-get 'text (aref (alist-get 'content
                                                    (alist-get 'result response)) 0))))
        (should (equal text "```emacs-lisp\nfo\"obar\n```"))))))

(ert-deftest emcp-tests-eval-tool-non-string-prin1 ()
  ;; A non-string result is printed with prin1, so a list of strings stays
  ;; unambiguous and re-readable.
  (let ((emcp-tools-eval-default-policy t))
    (emcp-test-with-tool-response response 'emcp-tools-eval
                                  '((code . "(list \"a\" \"b\")"))
      (let ((text (alist-get 'text (aref (alist-get 'content
                                                    (alist-get 'result response)) 0))))
        (should (equal text "```emacs-lisp\n(\"a\" \"b\")\n```"))))))

(ert-deftest emcp-tests-eval-tool-default-reject ()
  (let ((emcp-tools-eval-default-policy nil))
    (emcp-test-with-tool-response response 'emcp-tools-eval
                                  '((code . "(+ 1 2)"))
      (should (eq (alist-get 'isError (alist-get 'result response)) t)))))

(ert-deftest emcp-tests-eval-tool-malformed ()
  (let ((emcp-tools-eval-default-policy t))
    (emcp-test-with-tool-response response 'emcp-tools-eval
                                  '((code . "(unbalanced"))
      (let* ((result (alist-get 'result response))
             (text (alist-get 'text (aref (alist-get 'content result) 0))))
        (should (eq (alist-get 'isError result) t))
        (should (string-match-p "Unbalanced\\|incomplete" text))))))

(ert-deftest emcp-tests-eval-tool-runtime-error ()
  (let ((emcp-tools-eval-default-policy t))
    (emcp-test-with-tool-response response 'emcp-tools-eval
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
    (emcp-test-with-tool-response response 'emcp-tools-eval
                                  '((code . "(setq emcp-tests--probe 1) (1+ emcp-tests--probe)"))
      (let ((text (alist-get 'text (aref (alist-get 'content
                                                    (alist-get 'result response)) 0))))
        (should (equal text "```emacs-lisp\n2\n```"))
        (should (= emcp-tests--probe 1))))))

(provide 'emcp-tools-eval-tests)
;;; emcp-tools-eval-tests.el ends here
