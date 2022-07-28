;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; package system
;;------------------------------------------------------------------------------
(with-eval-after-load 'package
  (eval-when-compile (require 'package))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (setq package-menu-async nil
        package-quickstart t
        custom-file (eval-when-compile (concat user-emacs-directory "my-custom-file.el"))))

;;------------------------------------------------------------------------------
;; load files
;;------------------------------------------------------------------------------
(load "-packages-" t t)
(load "-major-mode-" t t)
