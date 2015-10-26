;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; flycheck 設定
;; エラーチェッカー
;; from package
;;------------------------------------------------------------------------------

;; remove useless warnings
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
