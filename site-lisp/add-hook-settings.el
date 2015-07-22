;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; add-hook-mode
;;------------------------------------------------------------------------------
;;-- デフォルトのメジャーモード --;;
(setq-default major-mode 'text-mode)
;
;;-- fileの関連付け --;;
;(setq auto-mode-alist (append (list
;                               '("\\.txt$" . text-mode)
;                              auto-mode-alist)))

;;-- 不要なhookを外す --;;
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)

;;-- mode設定 --;;
;; 共通設定 (有効化)
(defun common-mode-enable-hooks ()
  (setq tab-width 4)
  (whitespace-mode t)
  (linum-mode 1)
  (cua-mode t)
  (show-paren-mode t))
(dolist (hook '(text-mode-hook emacs-lisp-mode-hook
				sh-mode-hook makefile-mode-hook
				c-mode-common-hook))
  (add-hook hook 'common-mode-enable-hooks))

;; 共通設定 (無効化)
(defun common-mode-disable-hooks ()
  (whitespace-mode 0)
  (linum-mode 0)
  (cua-mode 0)
  (show-paren-mode 0))
(dolist (hook '(esup-mode-hook emacs-lisp-byte-code-mode-hook))
  (add-hook hook 'common-mode-disable-hooks))

;; プログラミング言語 共通設定 (有効化)
(defvar eldoc-idle-delay)
(defun common-programing-mode-enable-hooks ()
  (eldoc-mode t)
  (setq eldoc-idle-delay 0.5))
(dolist (hook '(emacs-lisp-mode-hook c-mode-common-hook))
  (add-hook hook 'common-programing-mode-enable-hooks))

;; プログラミング言語 共通設定 (無効化)
(defun common-programing-mode-disable-hooks ()
  (eldoc-mode nil)
  (setq eldoc-idle-delay 0.5))
(dolist (hook '(esup-mode-hook emacs-lisp-byte-code-mode-hook))
  (add-hook hook 'common-programing-mode-disable-hooks))

;; c,c++mode
(defun c_c++-mode-hooks ()
  (vs-set-c-style))
(autoload 'vs-set-c-style "vs-set-c-style")
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook 'c_c++-mode-hooks))

;; ヘッダファイルを C++ として認識させる
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

