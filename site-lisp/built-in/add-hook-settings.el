;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; add-hook
;;------------------------------------------------------------------------------

;;-- デフォルトのメジャーモード --;;
(setq-default major-mode 'text-mode)

;; -- 拡張子の関連付け --;;
(dolist (assoc-extension '(("\\.h$" . c++-mode)))
  (add-to-list 'auto-mode-alist assoc-extension))

;;-- 不要なhookを外す --;;
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)

;;-- mode設定 --;;
;; 共通
(defun common-enable-hooks ()
  (whitespace-mode +1) ;; whitespace
  (nlinum-mode +1) ;; nlinum
  (cua-mode +1) ;; cua
  (company-mode +1)) ;; company

;; プログラミング言語共通
(defun common-programing-enable-hooks ()
  (setq tab-width 4) ;; tab 幅
  (show-paren-mode +1) ;; 括弧のハイライト
  (setq truncate-lines +1) ;; 画面外文字の切り詰め
  (setq truncate-partial-width-windows +1)) ;; 縦分割時の画面外文字の切り詰め

;; eldoc
(defvar eldoc-idle-delay)
(defun eldoc-enable-hooks ()
  (eldoc-mode +1) ;; eldoc
  (setq eldoc-idle-delay 0.5)) ;; eldoc 遅延

;; flycheck
(defun flycheck-enable-hooks ()
  (setq left-fringe-width 8) ;; 左フリンジを有効化
  (flycheck-mode +1)) ;; flycheck

;; text-mode
(add-hook 'text-mode-hook 'common-enable-hooks)

;; emacs-lisp-mode
(defun emacs-lisp-enable-hooks ()
  (when (and buffer-file-name (string-match "\\.el$" buffer-file-name))
    (enable-auto-async-byte-compile-mode)
    (common-enable-hooks)
    (common-programing-enable-hooks)
    (eldoc-enable-hooks)
    (flycheck-enable-hooks)))
(autoload 'enable-auto-async-byte-compile-mode "auto-async-byte-compile" t)
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-enable-hooks)

;; c/c++-mode
(defun c/c++-enable-hooks ()
  (common-enable-hooks)
  (common-programing-enable-hooks)
  ;; (eldoc-enable-hooks)
  (flycheck-enable-hooks)
  (vs-set-c-style)
  (ggtags-mode +1) ;; ggtags
  (irony-mode +1)) ;; irony
(autoload 'vs-set-c-style "vs-set-c-style")
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook 'c/c++-enable-hooks))

;; sh-mode
(dolist (hook-func '(common-enable-hooks
                     common-programing-enable-hooks
                     ;;flycheck-enable-hooks
                     ))
  (add-hook 'sh-mode-hook hook-func))

;; makefile/css/js-mode
(dolist (hook '(makefile-mode-hook css-mode-hook js-mode-hook))
  (dolist (hook-func '(common-enable-hooks common-programing-enable-hooks))
    (add-hook hook hook-func)))
