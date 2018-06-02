;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; garbage collection
;;------------------------------------------------------------------------------
(setq gc-cons-threshold (* 128 1024 1024))

;;------------------------------------------------------------------------------
;; load path
;;------------------------------------------------------------------------------
(let ((default-directory (eval-when-compile (expand-file-name (concat user-emacs-directory "site-lisp")))))
  (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path))

;;------------------------------------------------------------------------------
;; package system
;;------------------------------------------------------------------------------
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(load "-proxy-" t t)

;;------------------------------------------------------------------------------
;; load files
;;------------------------------------------------------------------------------
;; -- win or linux -- ;;
(load (eval-when-compile (replace-regexp-in-string "/" "-" (format "-%s-misc-" system-type))) t t)

;; -- common --;;
(load (file-name-sans-extension (setq custom-file (locate-library "-common-misc-.el"))) t t)
(load "-local-function&macro-" t t)
(load "-major-mode-" t t)
(load "-packages-" t t)
(load "-shell-" t t)
