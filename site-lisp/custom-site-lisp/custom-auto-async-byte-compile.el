;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; site-lisp 設定
;;------------------------------------------------------------------------------
;; auto-async-byte-compile 設定
(with-eval-after-load 'lisp-mode
  (require 'auto-async-byte-compile)
  (defvar auto-async-byte-compile-init-file)
  (setq auto-async-byte-compile-init-file "~/.emacs.d/site-lisp/initfuncs.elc")
  (defvar auto-async-byte-compile-exclude-files-regexp)
  (setq auto-async-byte-compile-exclude-files-regexp "/elpa/*/*")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))
