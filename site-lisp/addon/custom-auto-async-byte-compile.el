;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; auto-async-byte-compile
;; バイトコンパイルの自動化
;; from : package system
;;------------------------------------------------------------------------------

(autoload 'enable-auto-async-byte-compile-mode "auto-async-byte-compile" t)
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
(with-eval-after-load 'auto-async-byte-compile
  (defvar auto-async-byte-compile-init-file)
  (setq auto-async-byte-compile-init-file "~/.emacs.d/init.elc")
  (defvar auto-async-byte-compile-exclude-files-regexp)
  (setq auto-async-byte-compile-exclude-files-regexp "/elpa/*/*"))
