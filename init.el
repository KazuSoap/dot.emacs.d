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
;; (eval-when-compile
;;   (defconst my-site-lisp-LastModifiedTime
;;     (nth 5 (file-attributes (concat user-emacs-directory "site-lisp")))))

(let ((default-directory (eval-when-compile (expand-file-name (concat user-emacs-directory "site-lisp")))))
  (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path))

;;------------------------------------------------------------------------------
;; package system
;;------------------------------------------------------------------------------
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (load "-proxy-" t t)
(setq custom-file
      (eval-when-compile
        (expand-file-name (concat user-emacs-directory "my-custom-file.el"))))

;;------------------------------------------------------------------------------
;; load files
;;------------------------------------------------------------------------------
;; -- win or linux misc -- ;;
(load (eval-when-compile (replace-regexp-in-string "/" "-" (format "-%s-misc-" system-type))) t t)

;; -- common --;;
(load "-local-function&macro-" t t)
(load "-major-mode-" t t)
(load "-packages-" t t)
(load "-shell-" t t)
