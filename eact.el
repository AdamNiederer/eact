;;; eact --- Write badass UIs in elisp -*- lexical-binding: t; -*-

;; Copyright 2017 Adam Niederer

;; Author: Adam Niederer <adam.niederer@gmail.com>
;; Keywords: tools maint lisp hypermedia

;; Version: 0.0.2
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

(defvar eact-ols #s(hash-table size 300 rehash-size 4.0 test equal))

(defun eact--keymap (keylist)
  "Make a keymap mapping even entries to odd entries from KEYLIST."
  (let ((map (make-sparse-keymap)))
    (dolist (item (-partition 2 keylist))
      (define-key map (kbd (car item))
        (lambda () (interactive) (funcall-interactively (cadr item)))))
    map))

(defun eact--render-node (props render)
  "Call RENDER and style its insertions with an overlay according to PROPS."
  (let ((overlay (make-overlay (point) (progn (funcall render) (point)))))
    (-let (((&plist :face :keymap :hover) props))
      (overlay-put overlay 'face face)
      (overlay-put overlay 'mouse-face hover)
      (overlay-put overlay 'keymap (eact--keymap keymap))
      (overlay-put overlay 'read-only t)
      (overlay-put overlay 'cursor-sensor-functions (list (lambda (_ _ entered) (overlay-put overlay 'face (if (and hover (equal entered 'entered)) hover face))))))
    (puthash (point) overlay (gethash (buffer-name) eact-ols))))

(defun eact--render-tree (tree)
  "Renders TREE if it's not an overlay, or renders its contents if it is."
  (cond
   ((stringp tree) ;; Base node; just insert it
    (insert tree))
   ((numberp tree) ;; Base node; convert and insert
    (insert (number-to-string tree)))
   ;; tree is assumed to be a list at this point
   ((stringp (car tree)) ;; Row node; insert all children, then "\n"
    (progn (--each tree (eact--render-tree it)) (insert "\n")))
   ;; (car tree) is assumed to be a list at this point
   ((keywordp (caar tree)) ;; Overlay; render subtree with style
    (eact--render-node (car tree) (lambda () (-each (cdr tree) #'eact--render-tree))))
   ;; (caar tree) is assumed to be a list at this point
   ((listp (caar tree)) ;; Row node with overlay; same as raw row node
    (progn (--each tree (eact--render-tree it)) (insert "\n")))))

(defun eact-render (buffer template)
  "Renders TEMPLATE into BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (when-let ((buffer-ols (gethash (buffer-name) eact-ols)))
        (dolist (point (hash-table-keys buffer-ols))
          (delete-overlay (gethash point buffer-ols))
          (remhash point buffer-ols)))
      (remhash (buffer-name) eact-ols)
      (puthash (buffer-name) (make-hash-table :size 300 :rehash-size 4.0 :test #'equal)
               eact-ols)
      (erase-buffer)
      (dolist (tree template)
        (eact--render-tree tree))
      (special-mode)
      (cursor-sensor-mode))))

(provide 'eact)

;;; eact.el ends here
