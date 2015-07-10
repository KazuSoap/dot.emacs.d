;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; garbage collection
;;------------------------------------------------------------------------------
(setq gc-cons-threshold (* 128 1024 1024))

;;------------------------------------------------------------------------------
;; package system
;;------------------------------------------------------------------------------
(package-initialize)
(defvar package-archives)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;------------------------------------------------------------------------------
;; load path
;;------------------------------------------------------------------------------
;; site-lisp
(let((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;;------------------------------------------------------------------------------
;; load files(local)
;;------------------------------------------------------------------------------
(load "custom-set-variables")
(load "fringe_modeline_buffer")
(load "color-theme")
(load "add-hook-mode")
(load "custom-site-lisp")
(load "general-key-bind")
(load "print")
(load "coding-system")
(with-eval-after-load 'shell (load "shell-mode") )
;(load "YaTeX-mode")
(load "calculate_bootup_time")

