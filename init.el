;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; garbage collection
;;------------------------------------------------------------------------------
(setq gc-cons-threshold (* 128 1024 1024))

;;------------------------------------------------------------------------------
;; package system
;;------------------------------------------------------------------------------
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/") t)

;;------------------------------------------------------------------------------
;; load path
;;------------------------------------------------------------------------------
(let ((default-directory (expand-file-name (concat user-emacs-directory "site-lisp"))))
  (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path))

;;------------------------------------------------------------------------------
;; load files
;;------------------------------------------------------------------------------
;; -- local functions & macro --
(load "-local-function&macro-")

;; -- win or linux -- ;;
(cond ((eq system-type 'windows-nt)
       (load "-win-misc-")
       (load "-fix-win-bugs-")
       (load "-print-"))
      ((eq system-type 'gnu/linux)
       (load "-linux-misc-")))

;; -- common --;;
(load "-company-")
(load "-elscreen-")
(load "-flycheck-")
(load "-helm-")
(load "-irony-") ;; depend on company & flycheck
(load "-migemo-")
(load "-twittering-mode-")
(load "-add-hook-")
(load "-aute-insert-")
(load "-gdb-")
(load "-fringe_modeline_buffer-")
(load "-general-key-bind-")
(load "-shell-")
(load (file-name-sans-extension (setq custom-file (locate-library "-common-misc-.el"))))
;; (with-eval-after-load 'kkc
;;   (load "-kkc-cmd-")
;;   (load "-kkc-popup-"))
