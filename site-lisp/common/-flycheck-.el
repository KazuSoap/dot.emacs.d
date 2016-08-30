;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; flycheck 設定
;; エラーチェッカー
;; from package
;;------------------------------------------------------------------------------

(defvar flycheck-display-errors-delay)  ;; 遅延
(setq flycheck-display-errors-delay 0.3)

;; remove useless warnings
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
