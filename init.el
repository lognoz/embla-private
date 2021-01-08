;;; init.el --- initialization file -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla-private
;; Keywords: init

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

(defconst embla-private-lisp-directory (expand-file-name "lisp/" embla-private-directory)
  "The directory of private lisp files.")

(defconst embla-private-lisp-autoloads-file (expand-file-name "embla-private-lisp-autoloads.el" embla-temporary-directory)
  "The private lisp autoloads file.")


;;; External function.

(defun embla-private-bootstrap ()
  "Bootstrap private Embla, if it hasn't already."
  (require-private-lisp-autoloads))

(defun refresh-private-lisp-autoloads ()
  "Update the autoloads located in `embla-private-lisp-directory'."
  (interactive)
  (refresh-autoloads embla-private-lisp-directory embla-private-lisp-autoloads-file))

(defun require-private-lisp-autoloads ()
  "Require autoloads located in `lisp' directory."
  (when (not (file-exists-p embla-private-lisp-autoloads-file))
    (require 'autoloads)
    (refresh-private-lisp-autoloads))
  (require 'embla-private-lisp-autoloads))


;; Bootstrap Embla configurations.
(embla-private-bootstrap)

;;; init.el ends here
