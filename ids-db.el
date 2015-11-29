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

(require 'ids-edit)
(require 'ids-equiv)
(require 'ids-normalize)

(eval-when-compile

  (defvar ids-table nil)
  (defvar ids-db-count 0)

  (defvar normalize-table nil)
  (defvar reverse-table nil)
  (defvar canonical-table nil)

  (defun ids-db-init ()
    "Initialize IDS table."
    (interactive)
    (message "Now initializing...")
    (setq canonical-table
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
                    (when (gethash char canonical-table)
                      (error "Canonical Conflict! %c" char))
                    (unless (equal single-char char)
                      (pushnew char
                               (gethash (string-to-char single-char) table)))))))
            table))
    (setq normalize-table
          (let ((table (make-hash-table :test 'equal)))
            (maphash (lambda (key val)
                       (puthash key (mapcar
                                     'ids-normalize-canonicalize
                                     (mapcar
                                      'ids-normalize-structure val)) table))
                     ids-table)
            table))
    (setq reverse-table
          (let ((table (make-hash-table :test 'equal)))
            (maphash (lambda (key val)
                       (dolist (ids val)
                         (pushnew key (gethash ids table))))
                     normalize-table)
            table))
    (setq ids-db-count 0)
    (while (ids-db-renormalize))
    (message "init done."))

  (defun ids-db-renormalize ()
    "Re-normalize `normalize-table'.
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
             (puthash char (nconc chars new-trees ) normalize-table))))
       normalize-table)
      ;; making IDS->char table.
      (setq reverse-table
            (let ((table (make-hash-table :test 'equal)))
              (maphash (lambda (key val)
                         (dolist (ids val)
                           (pushnew key (gethash ids table))))
                       normalize-table)
              table))
      (message "renormalize done (%d)." ids-db-count)
      (cl-incf ids-db-count)
      flag))

  (ids-db-init)
  )

(defvar ids-normalize-table
  (eval-when-compile normalize-table)
  "Normalized table of character to IDSes.")

(defvar ids-reverse-table
  (eval-when-compile reverse-table)
  "Normalized table of IDS to characters.")

(defvar ids-canonical-table
  (eval-when-compile canonical-table)
  "Table of characters to be canonicalized.")

(provide 'ids-db)

;;; ids-db.el ends here
