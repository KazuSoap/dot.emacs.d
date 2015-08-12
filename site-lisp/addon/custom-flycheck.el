;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; flycheck 設定
;; エラーチェッカー
;; from package
;;------------------------------------------------------------------------------

;; c++11 でのチェック
(defun flycheck-c++-mode-hooks ()
  (defvar flycheck-gcc-language-standard)
  (setq flycheck-gcc-language-standard "c++11")
)
(add-hook 'c++-mode-hook 'flycheck-c++-mode-hooks)

;; remove useless warnings
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

