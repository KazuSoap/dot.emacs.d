;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; migemo 設定
;;------------------------------------------------------------------------------
;;; migemo ---------------------------------------------------------------------
;; ローマ字入力で日本語文字列を検索
;; from : package system
(require 'migemo)

(setq migemo-command "d:/Program Files/cmigemo/cmigemo")
(setq migemo-options '("-q" "--emacs"))

;; Set your installed path
(setq migemo-dictionary (expand-file-name "d:/Program Files/cmigemo/dict/utf-8/migemo-dict"))
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)

(load-library "migemo")
(migemo-init)

;; local key-bind
(define-key isearch-mode-map (kbd "C-M-y") 'migemo-isearch-yank-char)
(define-key isearch-mode-map (kbd "C-w") 'migemo-isearch-yank-word)
(define-key isearch-mode-map (kbd "M-s C-e") 'migemo-isearch-yank-line)
(define-key isearch-mode-map (kbd "M-m") 'migemo-isearch-toggle-migemo)
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)

