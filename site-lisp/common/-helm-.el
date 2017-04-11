;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; helm
;; emacsに統一的なある”操作方式”を提供するフレームワーク
;; from : package system
;;------------------------------------------------------------------------------

(with-eval-after-load 'helm
  ;; helm-migemo を有効化
  (helm-migemo-mode 1)

  ;; http://fukuyama.co/nonexpansion
  ;; 自動補完を無効にする
  (defvar helm-ff-auto-update-initial-value)
  (setq helm-ff-auto-update-initial-value nil))

;; key-bind
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
