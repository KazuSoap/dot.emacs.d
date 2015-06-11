;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; site-lisp 設定
;;------------------------------------------------------------------------------
;; tabber 設定
;(load "custom-tabbar")

;elscreen 設定
(load "custom-elscreen")

;; auto-complete 設定
(with-eval-after-load 'auto-complete (load "custom-auto-complete"))

;; TRAMP 設定
(with-eval-after-load 'tramp (load "custom-tramp"))

;; migemo 設定
(load "custom-migemo")

;; helm 設定
(with-eval-after-load 'helm (load "custom-helm"))

;; auto-async-byte-compile 設定
(with-eval-after-load 'lisp-mode
  (require 'auto-async-byte-compile)
  (defvar auto-async-byte-compile-exclude-files-regexp "/junk/")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;; pop_window 設定
;(load "custom-pop_window")

;; twittering-mode 設定
;;(load "custom-twittering-mode")
