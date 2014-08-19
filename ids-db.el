;;; ids-db.el --- IDS Database Management -*- lexical-binding: t; -*-

;; Copyright (C) 2014  KAWABATA, Taichi

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

;; Construct IDS Database.

;;; Code:

(require 'ids-edit) ; external library

(require 'ids-normalize)
(require 'ids-equiv)

(defvar ids-table nil "IDS table.")
(defvar ids-db-count 0)

(defun ids--addhash (key value table)
  "Add to KEY a VALUE in table TABLE."
  (unless (member value (gethash key table))
    (let* ((old-value (gethash key table)))
      (if old-value (nconc old-value (list value))
        (puthash key (list value) table)))))

(defun ids-db-init ()
  "Initialize IDS table."
  (interactive)
  (message "Now initializing...")
  (setq ids-canonical-table
        (let ((table (make-hash-table :test 'equal)))
          (dolist (item ids-canonicals)
            (setq item (mapcar 'string-to-char item))
            (dolist (char (cdr item))
              (when (gethash char table) (error "Canonicals conflict!!! %c" char))
              (puthash char (car item) table)))
          table))
  (setq ids-table
        (let ((table (copy-hash-table ids-edit-table)))
          ;; special equivalnet chars and IDSes.
          (dolist (item ids-equivalents)
            (let ((single-chars (--filter (= 1 (length it)) item)))
              (dolist (single-char single-chars)
                (dolist (char item)
                  (when (gethash char ids-canonical-table)
                    (error "Canonical Conflict! %c" char))
                  (unless (equal single-char char)
                    (ids--addhash (string-to-char single-char) char table))))))
          table))
  (setq ids-normalize-table
        (let ((table (make-hash-table :test 'equal)))
          (maphash (lambda (key val)
                     (puthash key (mapcar
                                   'ids-normalize-canonicalize
                                   (mapcar
                                    'ids-normalize-structure val)) table))
                   ids-table)
          table))
  (setq ids-reverse-table
        (let ((table (make-hash-table :test 'equal)))
          (maphash (lambda (key val)
                     (dolist (ids val)
                       (ids--addhash ids key table)))
                   ids-normalize-table)
          table))
  (setq ids-db-count 0)
  (while (ids-db-renormalize))
  (message "init done."))

(defun ids-db-renormalize ()
  "Re-normalize `ids-normalize-table'.
Return t if table is actually changed."
  (interactive)
  (let (flag)
    (maphash
     (lambda (char ids-trees)
       (let* ((chars (--filter (characterp it) ids-trees))
              (trees (--filter (not (characterp it)) ids-trees))
              (renewed-flag)
              (new-trees
               (-uniq
                (--mapcat
                 (let ((normalized-trees (ids-normalize-rotate it)))
                   (if (member it normalized-trees) (list it)
                     (setq renewed-flag t)
                     normalized-trees))
                 trees))))
         (when renewed-flag
           (setq flag t)
           (puthash char (nconc chars new-trees ) ids-normalize-table))))
     ids-normalize-table)
    ;; making IDS->char table.
    (setq ids-reverse-table
          (let ((table (make-hash-table :test 'equal)))
            (maphash (lambda (key val)
                       (dolist (ids val)
                         (ids--addhash ids key table)))
                     ids-normalize-table)
            table))
    (message "renormalize done (%d)." ids-db-count)
    (cl-incf ids-db-count)
    flag))

(ids-db-init)

(provide 'ids-db)

;;; ids-db.el ends here
