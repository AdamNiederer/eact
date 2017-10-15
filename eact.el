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

(require 'elquery)
(require 'dash)

;; TODO: Track all overlays
(defvar eact-root-ol nil)

(defmacro fncase (fn expr &rest match-bodies)
  "Exactly like `case', but compares with FN; Match EXPR against MATCH-BODIES."
  (declare (indent 1))
  `(cond
    ,@(-map (lambda (match-body)
              `(,(cond
                  ((listp (car match-body))
                   `(or ,@(-map (lambda (match) `(funcall ,fn ,match ,expr)) (car match-body))))
                  ((equal (car match-body) 't) t)
                  (t `(funcall ,fn ,(car match-body) ,expr)))
                (progn
                  ,@(cdr match-body))))
            match-bodies)))

;; (macroexpand '(fncase 'equal 2 ((1 2 3) 2) (t 'kek)))

(defun eact-render-el-pre (start tree)
  "Render the element at START heading TREE's prefix."
  (fncase #'equal (elquery-el tree)
    ("div" (save-excursion (goto-char start) (insert "\n") (point)))
    (t start)))

(defun eact-render-el-post (start tree)
  "Render the element at START heading TREE's postfix."
  (fncase #'equal (elquery-el tree)
    ("div" (save-excursion (goto-char start) (insert "\n") (point)))
    (t start)))

(defun eact-render-tree (start tree)
  "START TREE."
  (save-excursion
    (if (elquery-el tree)
        (eact-render-ol
         tree start
         (eact-render-el-post
          (--reduce
           (let ((ol-or-pos (eact-render-tree acc it)))
             (if (overlayp ol-or-pos) (overlay-end ol-or-pos) ol-or-pos))
           (cons (eact-render-el-pre start tree) (elquery-children tree)))
          tree))
      (progn (goto-char start)
             (insert (elquery-text tree))
             (point)))))

(defun eact-render-ol (tree start end)
  "TREE START END."
  (let ((ol (make-overlay start end)))
    ;; (overlay-put ol 'face '(:background "#00ffff" :height 1.5))
    ;; (overlay-put ol 'read-only t)
    ol))

(defun eact-render (buffer start template)
  "BUFFER START TEMPLATE."
  (with-current-buffer buffer
    (let ((tree (car (elquery-$ "template" (elquery-read-string template)))))
      (when eact-root-ol
        (when (overlay-start eact-root-ol)
          (delete-region (overlay-start eact-root-ol)
                         (overlay-end eact-root-ol)))
        (delete-overlay eact-root-ol)
        (delete-all-overlays)
        (setq eact-root-ol nil))
      (save-excursion
        (goto-char start)
        (setq eact-root-ol (eact-render-tree start tree))))))

;; (eact-render (get-buffer-create "*eact*") (point-min)
;;              "<template @kek=\"bur\">hello!<div>world<asdf/>...</div>asdfu&#10;&#13;</template>")

(provide 'eact)

;;; eact.el ends here
