;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; misc
;;------------------------------------------------------------------------------
;; cursor点滅表示 (default)
;; (blink-cursor-mode -1)

;;------------------------------------------------------------------------------
;; package system
;;------------------------------------------------------------------------------
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-menu-async nil)

;;------------------------------------------------------------------------------
;; load files
;;------------------------------------------------------------------------------
;; -- win or linux misc -- ;;
(load (eval-when-compile (replace-regexp-in-string "/" "-" (format "-%s-misc-" system-type))) t t)

;; -- common --;;
(load "-major-mode-" t t)
(load "-packages-" t t)
