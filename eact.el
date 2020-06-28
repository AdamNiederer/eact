;;; eact --- Write badass UIs in elisp -*- lexical-binding: t; -*-

;; Copyright 2017 Adam Niederer

;; Author: Adam Niederer <adam.niederer@gmail.com>
;; Keywords: tools maint lisp hypermedia

;; Version: 0.0.1
;; Package-Requires: ((elquery "0.1.0") (dash "2.13.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'ol)
(require 'elquery)

(defvar eact-ols #s(hash-table size 300 rehash-size 4.0))

(defun eact--keymap (keylist)
  "Make a keymap mapping even entries to odd entries from KEYLIST."
  (let ((map (make-sparse-keymap)))
    (dolist (item (-partition 2 keylist))
      (define-key map (kbd (car item))
        (lambda () (interactive) (funcall-interactively (cadr item)))))
    map))

(defun eact--render-cmd (cmd)
  "Insert text and create overlays in the current buffer as described in CMD."
  (let ((overlay (make-overlay (point) (progn (insert (car cmd)) (point)))))
    (overlay-put overlay 'face (plist-get (cdr cmd) :face))
    (overlay-put overlay 'keymap (eact--keymap (plist-get (cdr cmd) :keymap)))
    (overlay-put overlay 'read-only t)
    (puthash (point) overlay (gethash (buffer-name) eact-ols))))

(defun eact--render-row (row)
  "Renders ROW on one line of the current buffer."
  (dolist (cmd row)
    (cond
     ((stringp cmd) (eact--render-cmd (list cmd)))
     ((listp cmd) (eact--render-cmd cmd)))))

(defun eact-render (buffer template)
  "Renders TEMPLATE into BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (when-let ((buffer-ols (gethash (buffer-name) eact-ols)))
        (dolist (point (hash-table-keys buffer-ols))
          (delete-overlay (gethash point buffer-ols))
          (remhash point buffer-ols)))
      (remhash (buffer-name) eact-ols)
      (puthash (buffer-name) #s(hash-table size 300 rehash-size 4.0) eact-ols)
      (erase-buffer)
      (dolist (row template)
        (eact--render-row row)
        (insert "\n")))))

(provide 'eact)

;;; eact.el ends here
