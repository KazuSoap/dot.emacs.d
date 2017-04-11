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

(defun ad-migemo-register-isearch-keybinding ()
  (define-key isearch-mode-map (kbd "C-M-y") 'migemo-isearch-yank-char)
  (define-key isearch-mode-map (kbd "C-w") 'migemo-isearch-yank-word)
  (define-key isearch-mode-map (kbd "M-s C-e") 'migemo-isearch-yank-line)
  (define-key isearch-mode-map (kbd "M-m") 'migemo-isearch-toggle-migemo)
  (define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill))
(advice-add 'migemo-register-isearch-keybinding :override 'ad-migemo-register-isearch-keybinding)

(require 'cmigemo)
