;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; IME
;;------------------------------------------------------------------------------
;; emacs-mozc
(setq default-input-method "japanese-mozc")

(with-eval-after-load 'mozc
  (require 'mozc-popup)
  (setq mozc-candidate-style 'popup))

(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
(global-set-key (kbd "<henkan>") (lambda () (interactive) (activate-input-method default-input-method)))
(global-set-key (kbd "<muhenkan>") (lambda () (interactive) (deactivate-input-method)))

;;------------------------------------------------------------------------------
;; misc
;;------------------------------------------------------------------------------
(setq select-enable-clipboard t)

;;------------------------------------------------------------------------------
;; irony
;; A C/C++ minor mode powered by libclang
;; from package
;;------------------------------------------------------------------------------

(with-eval-after-load 'irony
  ;; irony-server-install に失敗する問題の修正用
  (defvar ex-irony--install-server-read-cmd "\\1 -DLIBCLANG_LIBRARY=/usr/lib/llvm-3.8/lib/libclang.so"))
