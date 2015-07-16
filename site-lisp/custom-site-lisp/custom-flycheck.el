;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; flycheck 設定
;; コードチェッカー
;; from package
;;------------------------------------------------------------------------------

;; 特定のモードのみ flycheck-mode と左フリンジを有効
(dolist (hook '(emacs-lisp-mode-hook c-mode-hook c++-mode-hook))
  (add-hook hook '(lambda ()
					(setq left-fringe-width 8) ;; 左フリンジを有効化
					(flycheck-mode 0)
					(flycheck-mode t))))

;; 特定のモードで無効化
(dolist (hook '(lisp-interaction-mode-hook emacs-lisp-byte-code-mode-hook))
  (add-hook hook '(lambda ()
					(flycheck-mode 0))))
