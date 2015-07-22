;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; flycheck 設定
;; エラーチェッカー
;; from package
;;------------------------------------------------------------------------------
;; 特定のモードで自動的に有効化
(defun flycheck-mode-enable-hooks ()
  (setq left-fringe-width 8) ;; 左フリンジを有効化
  (flycheck-mode 0)
  (flycheck-mode t))
(dolist (hook '(emacs-lisp-mode-hook sh-mode-hook
				c-mode-common-hook))
  (add-hook hook 'flycheck-mode-enable-hooks))

;; 特定のモードで無効化
(defun flycheck-mode-disable-hooks ()
  (flycheck-mode 0))
(dolist (hook '(lisp-interaction-mode-hook emacs-lisp-byte-code-mode-hook))
  (add-hook hook 'flycheck-mode-disable-hooks))

;; c++11 でのチェック
(defun flycheck-c++-mode-hooks ()
  (defvar flycheck-gcc-language-standard)
  (setq flycheck-gcc-language-standard "c++11")
)
(add-hook 'c++-mode-hook 'flycheck-c++-mode-hooks)

