;;; lisp/tools/architect.el --- architect configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla-private
;; Keywords: architect

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

;;;###autoload
(eval-before-init
  ;; Provide functionality to create project template quickly.
  (unless (directory-in-site-p "architect")
    (clone-repository "https://github.com/lognoz/architect"))

  ;; Templates for architect.
  (unless (directory-in-site-p "architect-template")
    (clone-repository "https://github.com/lognoz/architect-template")))

;;;###autoload
(setq architect-directory
  (expand-file-name "architect-template" embla-site-lisp-directory))

;;; architect.el ends here
