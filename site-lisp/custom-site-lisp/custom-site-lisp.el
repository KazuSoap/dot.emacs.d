;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; site-lisp 設定
;;------------------------------------------------------------------------------
;elscreen 設定
(load "custom-elscreen")

;; auto-complete 設定
(load "custom-auto-complete")

;; flycheck 設定
(dolist (hook '(emacs-lisp-mode-hook c-mode-hook c++-mode-hook))
  (add-hook hook '(lambda ()
					(setq left-fringe-width 8) ;; 左フリンジを有効化
					(flycheck-mode nil)
					(flycheck-mode t))))

(dolist (hook '(lisp-interaction-mode-hook emacs-lisp-byte-code-mode-hook))
  (add-hook hook '(lambda ()
					(flycheck-mode nil))))

;; TRAMP 設定
(with-eval-after-load 'tramp (load "custom-tramp"))

;; migemo 設定
(load "custom-migemo")

;; helm 設定
(with-eval-after-load 'helm (load "custom-helm"))

;; auto-async-byte-compile 設定
(with-eval-after-load 'lisp-mode
  (require 'auto-async-byte-compile)
  (defvar auto-async-byte-compile-init-file)
  (setq auto-async-byte-compile-init-file "~/.emacs.d/site-lisp/initfuncs.elc")
  (defvar auto-async-byte-compile-exclude-files-regexp)
  (setq auto-async-byte-compile-exclude-files-regexp "/elpa/*/*")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;; sticky shift 設定
(require 'sticky)
; for japanese keyboards
(use-sticky-key ?\; sticky-alist:ja)

;; shell-pop 設定
(global-set-key [f8] 'shell-pop)
(with-eval-after-load 'shell-pop
  (defvar shell-pop-internal-mode)
  (setq shell-pop-internal-mode "shell")

  (defvar shell-pop-internal-mode-buffer)
  (setq shell-pop-internal-mode-buffer "*shell-pop: shell*")

  (defvar shell-pop-internal-mode-func)
  (setq shell-pop-internal-mode-func (lambda () (shell)))

  (defvar shell-pop-window-size)
  (setq shell-pop-window-size 30)

  (defvar shell-pop-full-span)
  (setq shell-pop-full-span t)

  (defvar shell-pop-window-position)
  (setq shell-pop-window-position "bottom"))

;; pop_window 設定
;; (load "custom-pop_window")

;; twittering-mode 設定
;; (load "custom-twittering-mode")
