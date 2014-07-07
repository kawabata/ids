;;; ids-normalize.el --- IDS Normalization Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2014 KAWABATA, Taichi

;; Author: KAWABATA, Taichi <kawabata.taichi@gmail.com>
;; Keywords: tools

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

;; A tool to normalize IDS string.

;;; Code:

(require 'ids)
(require 'dash)

;; following variables are initialized by ids-db.el
(defvar ids-normalize-table nil)
(defvar ids-reverse-table nil)
(defvar ids-canonical-table nil)

(defun ids-normalize-structure (ids)
  "Normalize IDS string.
Return IDS tree."
  (setq ids (replace-regexp-in-string "[⿵⿸⿹]" "⿱" ids))
  (setq ids (replace-regexp-in-string "[⿷⿺]" "⿰" ids))
  (setq ids (ids-tree-structure ids))
  (cl-labels
      ((proc (it)
             (cond ((not (listp it)) it)
                   ((equal (car it) ?⿶)
                    (list ?⿱ (proc (elt it 2)) (proc (elt it 1))))
                   ((equal (car it) ?⿲)
                    `(?⿰ ,(proc (elt it 1))
                          (?⿰ ,(proc (elt it 2)) ,(proc (elt it 3)))))
                   ((equal (car it) ?⿳)
                    `(?⿱ ,(proc (elt it 1))
                          (?⿱ ,(proc (elt it 2)) ,(proc (elt it 3)))))
                   (t (mapcar #'proc it)))))
    (proc ids)))

(defun ids-normalize-canonicalize (ids-tree)
  "Replace characters in IDS-TREE to canonical ones."
  (--tree-map
   (let ((decomposition (car (get-char-code-property it 'decomposition))))
     (if (and (not (equal decomposition it)) (characterp decomposition))
         decomposition
       (-if-let (canon (gethash it ids-canonical-table))
           canon
         it)))
     ids-tree))

(defun ids-normalize-rotate (ids-tree)
  "Rotate IDS-TREE.  Return ids-trees."
  (if (characterp ids-tree) (error "Not Tree! %s" ids-tree))
  (let ((head (car ids-tree))
        (left (elt ids-tree 1))
        (right (elt ids-tree 2))
        left-trees)
    ;; shrink left tree if possible.
    (setq left-trees
          (if (and (listp left)
                   (not (equal head (car left))))
              (ids-normalize-shrink left)
            (list left)))
    ;; expand left tree if possible
    (setq left-trees
          (-uniq
           (--mapcat
            (if (characterp it) (ids-normalize-expand it head) (list it)) left-trees)))
    ;; rotate
    (-uniq
     (--mapcat
      (if (and (listp it) (equal (car it) head))
          (ids-normalize-rotate `(,head ,(elt it 1) (,head ,(elt it 2) ,right)))
        (mapcar (lambda (right) (list head it right))
                (ids-normalize-shrink right)))
      left-trees))))

(defun ids-normalize-shrink (ids-tree)
  "Shrink an IDS-TREE.  Return a list of possible IDS trees.
e.g. '⿰氵毎' → (海),
     '⿺走斗' → ('﨣' '𧺯')"
  (if (characterp ids-tree)
      (--filter (characterp it)
                (-uniq (cons ids-tree (gethash ids-tree ids-normalize-table))))
    (let ((ids-trees (ids-normalize-rotate ids-tree)))
      (or
       (-uniq
        (--mapcat (gethash it ids-reverse-table)
                  ids-trees))
       ids-trees))))

(defun ids-normalize-expand (char ids-head)
  "Return IDS trees of CHAR which can be expanded with IDS-HEAD.
If it can't be expanded, return list of CHAR."
  (or
   (--filter
    (and (listp it) (equal (car it) ids-head))
    (-uniq
     (--mapcat
      (if (characterp it) (gethash it ids-normalize-table) (list it))
      (gethash char ids-normalize-table))))
   (list char)))

(defun ids-normalize (ids)
  "Normalize IDS string.
Returns a list of IDS string."
  (interactive)
  (let* ((ids-tree
          (ids-normalize-canonicalize
           (ids-normalize-structure ids)))
         (ids-trees (ids-normalize-shrink ids-tree)))
    (ids-normalize-to-strings ids-trees)))

(defun ids-normalize-to-strings (ids-trees)
  "Convert IDS-TREES to a list of String."
  (--map (apply 'string (-flatten it)) ids-trees))

(provide 'ids-normalize)
;;; ids-normalize.el ends here
