;;; ids-test.el --- test program for IDS tools. -*- lexical-binding: t; -*-

;; Copyright (C) 2014  KAWABATA, Taichi

;; Author: KAWABATA, Taichi <kawabata.taichi@gmail.com>

;;; Commentary:

;; % cask exec ert-runner -L .

;;; Code:

(require 'ert)
(require 'bytecomp)
(require 'ids-db)
(require 'dash)

(ids-db-init)

;; ids.el
(ert-deftest ids-split-string ()
  (should (equal (ids-split-string "⿰氵⿱艹⿻口夫⿱宀子文")
                 '("⿰氵⿱艹⿻口夫" "⿱宀子" "文"))))

(ert-deftest ids-tree-structure ()
  (should (equal (ids-tree-structure "⿰氵⿱𠂉母")
                 '(?⿰ ?氵 (?⿱ ?𠂉 ?母)))))

;; ids-normalize.el
(ert-deftest ids-normalize-structure ()
  (should (equal (apply 'string (ids-normalize-structure "⿶AB"))
                 "⿱BA")))

(ert-deftest ids-normalize-rotate ()
  (should (equal (car (ids-normalize "⿰⿰⿰⿰ABCD⿱⿱⿱⿱EFGHI"))
                 "⿰A⿰B⿰C⿰D⿱E⿱F⿱G⿱HI")))

(ert-deftest ids-normalize-shrink ()
  (should (-reduce
           '-intersection
           `(,(ids-normalize "⿱⿳士冖王心")
             ,(ids-normalize "⿱⿳士冖一志")))))

(ert-deftest ids-normalize-all ()
  (should (-reduce
           '-intersection
           `(,(ids-normalize "⿰⿱双双攵")
             ,(ids-normalize "⿰㕛𢼅")
             ,(ids-normalize "⿲㕛㕛攵")
             ,(ids-normalize "⿰叕攵")
             ("敪")))))

(provide 'ids-test)

;;; ids-test.el ends here
