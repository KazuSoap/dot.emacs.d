;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; misc
;;------------------------------------------------------------------------------
;; cursor点滅表示 (default)
;; (blink-cursor-mode -1)

;; 起動時間を [ms] 単位で表示
(add-hook 'after-init-hook
          (lambda ()
            (let* ((sec (string-to-number (emacs-init-time)))
                   (ms (* 1000 sec)))
              (message "Emacs loaded in %.3f ms" ms))))

;;------------------------------------------------------------------------------
;; load path
;;------------------------------------------------------------------------------
(let ((default-directory (eval-when-compile (concat user-emacs-directory "site-lisp"))))
  (normal-top-level-add-subdirs-to-load-path))

;;------------------------------------------------------------------------------
;; package system
;;------------------------------------------------------------------------------
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq custom-file (eval-when-compile (concat user-emacs-directory "my-custom-file.el")))
(setq package-menu-async nil)

;;------------------------------------------------------------------------------
;; load files
;;------------------------------------------------------------------------------
;; -- win or linux misc -- ;;
(load (eval-when-compile (replace-regexp-in-string "/" "-" (format "-%s-misc-" system-type))) t t)

;; -- common --;;
(load "-major-mode-" t t)
(load "-packages-" t t)
