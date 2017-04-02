;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; migemo
;; ローマ字入力で日本語文字列を検索
;; from : package system
;;------------------------------------------------------------------------------

(cond ((eq system-type 'windows-nt)
       (require 'cmigemo))
      ((eq system-type 'gnu/linux)
       (require 'migemo)

       (defvar migemo-command)
       (setq migemo-command "cmigemo")

       (defvar migemo-dictionary)
       (setq migemo-dictionary (expand-file-name "/usr/share/cmigemo/utf-8/migemo-dict"))

       (defvar migemo-options)
       (setq migemo-options '("-q" "--emacs" "-i" "\g"))

       (defvar migemo-user-dictionary)
       (setq migemo-user-dictionary nil)

       (defvar migemo-regex-dictionary)
       (setq migemo-regex-dictionary nil)

       (defvar migemo-coding-system)
       (setq migemo-coding-system 'utf-8-unix)

       (migemo-init)))
