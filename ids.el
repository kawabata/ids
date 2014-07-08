;;; ids.el --- IDS (Ideographic Description Sequence) processing utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2014 KAWABATA, Taichi

;; Author: KAWABATA, Taichi <kawabata.taichi@gmail.com>
;; Keywords: data

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

;; Basic functions for IDS (Ideographic Description Sequence) processing

;;; Code:

(require 'dash)

(declare-function ids-debug "ids-debug")

(defun ids-forward-char ()
  "If there is an IDS at the point in buffer, forward the point to next.
Return the IDS tree structure."
  (interactive)
  (cond ((looking-at "[⿰⿱⿴⿵⿶⿷⿸⿹⿺⿻]")
         (forward-char)
         (list (char-before (point))
               (ids-forward-char) (ids-forward-char)))
        ((looking-at "[⿲⿳]")
         (forward-char)
         (list (char-before (point))
               (ids-forward-char) (ids-forward-char) (ids-forward-char)))
        ((eobp) (error "Incomplete IDS! %s" (buffer-string)))
        (t (forward-char) (char-before (point)))))

(defun ids-split-string (string)
  "Split STRING by IDS."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let (result (point (point)))
      (while (not (eobp))
        (ids-forward-char)
        (push (buffer-substring point (point)) result)
        (setq point (point)))
      (nreverse result))))

(defun ids-tree-structure (ids)
  "Tree structure of IDS string."
  (with-temp-buffer
    (insert ids)
    (goto-char (point-min))
    (ids-forward-char)))

(provide 'ids)
;;; ids.el ends here
