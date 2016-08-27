;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; general key bind
;;------------------------------------------------------------------------------

;; C-hでBS, shift+C-hでHelp
(keyboard-translate ?\C-h ?\C-?) ; translate `C-h' to DEL
(keyboard-translate ?\C-? ?\C-h)  ; translate DEL to `C-h'

(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm) ; reload buffer
(global-set-key (kbd "C-c C-r") 'my-window-resizer)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-z C-e") 'helm-elscreen)

;; migemo ((require 'migemo) する前に実行すべし!!)
(defun ad-migemo-register-isearch-keybinding ()
  (define-key isearch-mode-map (kbd "C-M-y") 'migemo-isearch-yank-char)
  (define-key isearch-mode-map (kbd "C-w") 'migemo-isearch-yank-word)
  (define-key isearch-mode-map (kbd "M-s C-e") 'migemo-isearch-yank-line)
  (define-key isearch-mode-map (kbd "M-m") 'migemo-isearch-toggle-migemo)
  (define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill))
(advice-add 'migemo-register-isearch-keybinding :override 'ad-migemo-register-isearch-keybinding)

;; shell-pop
(global-set-key [f8] 'shell-pop)

;; smart-compile
(global-set-key (kbd "C-x c") 'smart-compile)
