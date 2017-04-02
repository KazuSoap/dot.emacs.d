;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; migemo
;; ローマ字入力で日本語文字列を検索
;; from : package system
;;------------------------------------------------------------------------------

(defvar migemo-directory)
(setq migemo-directory
      (cond ((eq system-type 'windows-nt)
             "d:/msys64/usr/local/share/cmigemo")
            ((eq system-type 'gnu/linux)
             "/usr/share/cmigemo")))

(defvar migemo-dictionary)
(setq migemo-dictionary
  (concat migemo-directory "/utf-8/migemo-dict"))

(require 'cmigemo)
