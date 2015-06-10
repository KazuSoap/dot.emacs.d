;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; popup window 設定
;;------------------------------------------------------------------------------
;;; popwin ---------------------------------------------------------------------
;; ヘルプバッファや補完バッファをポップアップで表示
;; from : package system
(require 'popwin)
(popwin-mode 1)
(add-to-list 'popwin:special-display-config '("*Compile-Log*" :height 0.3 :noselect t))

;;; shell-pop ------------------------------------------------------------------
;; shell をポップウィンドウで読み込む
;; from : package system
(with-eval-after-load "shell-pop"
  (setq shell-pop-autocd-to-working-dir nil)
  (setq shell-pop-shell-type (quote ("shell" "*shell*" (lambda nil (shell)))))
  (setq shell-pop-term-shell "bash")
  (setq shell-pop-window-height 30)
  (setq shell-pop-window-position "bottom"))
