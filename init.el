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
(load "-common-misc-" t t)
(load "-major-mode-" t t)
(load "-local-function&macro-" t t)
(load "-packages-" t t)
(load "-shell-" t t)

;;------------------------------------------------------------------------------
;; custom-set-*
;;------------------------------------------------------------------------------
;; custom-save-all された直後に custom-file をバイトコンパイル
(declare-function custom-file "cus-edit")
(fset 'ad-custom-save-all (lambda() (byte-compile-file (custom-file))))
(advice-add 'custom-save-all :after 'ad-custom-save-all)

;; emacs 自動追記
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-semantic company-capf company-files
                  (company-dabbrev-code company-etags company-keywords)
                  company-oddmuse company-dabbrev)))
 '(package-selected-packages
   (quote
    (migemo flycheck-plantuml plantuml-mode company flycheck irony yasnippet twittering-mode smart-compile nlinum magit helm ggtags flycheck-irony exec-path-from-shell esup elscreen company-irony-c-headers company-irony))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
