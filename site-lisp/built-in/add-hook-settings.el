;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; add-hook-mode
;;------------------------------------------------------------------------------

;;-- デフォルトのメジャーモード --;;
(setq-default major-mode 'text-mode)

;; -- 拡張子の関連付け --;;
(setq auto-mode-alist
	  (append '(("\\.h$" . c++-mode))
			  auto-mode-alist))

;;-- 不要なhookを外す --;;
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)

;;-- mode設定 --;;
;; 共通設定
(defun common-mode-enable-hooks ()
  (setq tab-width 4) ;; tab 幅
  (whitespace-mode t) ;; whitespace
  (linum-mode 1) ;; linum
  (cua-mode t) ;; cua
  (show-paren-mode t)) ;; 括弧のハイライト
(dolist (hook '(text-mode-hook emacs-lisp-mode-hook
				sh-mode-hook makefile-mode-hook
				c-mode-common-hook))
  (add-hook hook 'common-mode-enable-hooks))

;; プログラミング言語 共通設定
(defvar eldoc-idle-delay)
(defun common-programing-mode-enable-hooks ()
  (setq truncate-lines t) ;; 画面外文字の切り詰め
  (setq truncate-partial-width-windows t) ;; 縦分割時の画面外文字の切り詰め
  (eldoc-mode t) ;; eldoc
  (setq eldoc-idle-delay 0.5)) ;; eldoc 遅延
(dolist (hook '(emacs-lisp-mode-hook c-mode-common-hook))
  (add-hook hook 'common-programing-mode-enable-hooks))

;; c,c++mode
(defun c_c++-mode-hooks ()
  (vs-set-c-style))
(autoload 'vs-set-c-style "vs-set-c-style")
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook 'c_c++-mode-hooks))

;; 共通設定 (無効化)
(defun common-mode-disable-hooks ()
  (setq truncate-lines nil)
  (setq truncate-partial-width-windows nil)
  (whitespace-mode 0)
  (linum-mode 0)
  (cua-mode 0)
  (show-paren-mode 0)
  (eldoc-mode 0))
(dolist (hook '(esup-mode-hook emacs-lisp-byte-code-mode-hook))
  (add-hook hook 'common-mode-disable-hooks))

