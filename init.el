;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; misc
;;------------------------------------------------------------------------------
;; startup-message off
;; site-start で設定不可 (ref: normal-top-level in startup.el)
(setq-default inhibit-startup-screen t)

;;------------------------------------------------------------------------------
;; load path
;;------------------------------------------------------------------------------
(let ((default-directory (eval-when-compile (concat user-emacs-directory "site-lisp"))))
  (normal-top-level-add-subdirs-to-load-path))

;;------------------------------------------------------------------------------
;; package system
;;------------------------------------------------------------------------------
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq-default custom-file (eval-when-compile (concat user-emacs-directory "my-custom-file.el")))
(setq-default package-menu-async nil)

;;------------------------------------------------------------------------------
;; load files
;;------------------------------------------------------------------------------
;; -- win or linux misc -- ;;
(load (eval-when-compile (replace-regexp-in-string "/" "-" (format "-%s-misc-" system-type))) t t)

;; -- common --;;
(load "-major-mode-" t t)
(load "-packages-" t t)
