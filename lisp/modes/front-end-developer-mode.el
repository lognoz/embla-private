;;; lisp/modes/front-end-developer-mode.el --- front end developer -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla-private
;; Keywords: front end

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)

;;; Contextual constant.

(defgroup front-end-developer nil
  "Provide more human-friendly front-end language development environment."
  :prefix "front-end-developer-"
  :group 'tools)

;;; Internal functions.

(defun front-end-developer--read (&rest plist)
  "Provide string input by defined PLIST.
It require an non-empty string before to return it."
  (let* ((answer) (valid) (prompt-text) (prompt-error-text)
         (candidates (plist-get plist :candidates))
         (input (plist-get plist :input))
         (input-error (plist-get plist :input-error))
         (regex (plist-get plist :regex))
         (prompt (plist-get plist :prompt)))
    (unless regex
      (setq regex ".+"))
    (unless prompt
      (setq prompt
        (if candidates 'completing-read 'read-string)))
    (unless input-error
      (setq input-error "require"))
    (setq prompt-text (concat input ": ")
          prompt-error-text
            (format "%s (%s): " input
                    (propertize input-error 'face
                                '((t :foreground "#ce5555")))))
    (while (not valid)
      (setq answer
        (string-trim
          (funcall prompt prompt-text
            (if (equal prompt 'completing-read)
                candidates
              answer))))
      (if (string-match-p regex answer)
          (setq valid t)
        (setq prompt-text prompt-error-text)))
    answer))

(defun front-end-developer--root-directory ()
  "Return project root directory."
  (locate-dominating-file
    (file-name-as-directory (file-name-directory buffer-file-name)) ".git"))


(defun front-end-developer-scss-include-screen--candidates (alist)
  "Return formatted ALIST for `completing-read' function."
  (let ((candidates))
    (dolist (parameters alist)
      (let* ((value (car parameters))
             (name (format "%s (%spx)" value (cdr parameters))))
        (setq candidates (push (cons name value) candidates))))
    candidates))

(defun front-end-developer-scss-add-to-breakpoints-variables (breakpoint scss-variables path breakpoints)
  "Insert new BREAKPOINT to its SCSS-VARIABLES loacted in PATH.
This function expects to receives a list of already defined BREAKPOINTS."
  (let* ((size (front-end-developer--read
                :regex "^[0-9]+$"
                :input "Size"
                :input-error "number only"))
         (new-breakpoint (cons breakpoint (string-to-number size))))
    (if breakpoints
        (setf (cdr (assoc 'breakpoints scss-variables))
          (push new-breakpoint breakpoints))
      (setq scss-variables (push (cons 'breakpoints (list new-breakpoint)) scss-variables)))
    (with-temp-buffer
      (insert (json-encode-alist scss-variables))
      (json-pretty-print-buffer)
      (write-region 1 (point-max) path))))

;;; External functions.

(defun front-end-developer-scss-include-screen ()
  "Return mixin with predefined breakpoints variable."
  (let* ((root-directory (front-end-developer--root-directory))
         (scss-path (expand-file-name "scss/variables.json" root-directory))
         (scss-variables)
         (breakpoints)
         (candidates)
         (answer)
         (screen))
    (unless (file-exists-p scss-path)
      (user-error "front-end-developer: Unable to find %s" scss-path))
    (setq scss-variables (json-content scss-path))
    (setq breakpoints
      (when (assoc 'breakpoints scss-variables)
        (cdr (assoc 'breakpoints scss-variables))))
    (when breakpoints
      (setq candidates (front-end-developer-scss-include-screen--candidates breakpoints)))
    (setq answer
      (front-end-developer--read
                      :input "Reference"
                      :candidates candidates
                      :prompt 'completing-read))
    (setq screen
      (if (assoc answer candidates)
          (cdr (assoc answer candidates))
        (front-end-developer-scss-add-to-breakpoints-variables
          answer
          scss-variables
          scss-path
          breakpoints)
        answer))
    (format "@include screen ('%s') {\n\t$0\n}" screen)))

;;;###autoload
(define-minor-mode front-end-developer-mode
  "Mode for front-end languages development."
  :lighter " front-end-developer"
  :global t)


(provide 'front-end-developer)

;;; front-end-developer.el ends here
