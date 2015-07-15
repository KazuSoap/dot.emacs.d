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
(load "initfuncs")

(dolist (loadfile '("custom-set-variables"
					"fringe_modeline_buffer"
					"custom-set-faces"
					"add-hook-settings"
					"custom-site-lisp"
					"general-key-bind"
					"print"
					"coding-system"
					"shell-settings"
					"calculate_bootup_time"))
  (unless (load loadfile t)
	(display-loading-error-message loadfile)))
