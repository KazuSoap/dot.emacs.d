;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; auto-async-byte-compile
;; バイトコンパイルの自動化
;; from : package system
;;------------------------------------------------------------------------------

(with-eval-after-load 'auto-async-byte-compile
  (defvar auto-async-byte-compile-init-file)
  (setq auto-async-byte-compile-init-file "~/.emacs.d/init.elc")
  (defvar auto-async-byte-compile-exclude-files-regexp)
  (setq auto-async-byte-compile-exclude-files-regexp "/elpa/*/*"))
