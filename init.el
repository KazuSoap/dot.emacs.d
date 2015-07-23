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
;; local functions
;;------------------------------------------------------------------------------
(defun display-loading-error-message (filename)
  "Display Loading Error Message"
  (let ((buffer (get-buffer-create "*Loading Error*")))
	(set-buffer buffer)
	(insert "cannot load \"" filename "\"\n")
	(display-buffer (current-buffer))))

(defun autoload-if-found (function file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (and (locate-library file)
       (autoload function file docstring interactive type)))

(defun cygpath (option &rest args)
  "cygpath for emacs"
  (let ((command (mapconcat #'identity (append (list "cygpath" option) args) "\s")))
	(substring (directory-file-name (shell-command-to-string command)) 0 -1)))

;;------------------------------------------------------------------------------
;; load files(local)
;;------------------------------------------------------------------------------
(dolist (loadfile '(;;-- addon --;;
					"custom-auto-async-byte-compile" ;; auto-async-byte-compile 設定
					"custom-auto-complete" ;; auto-complete 設定
					"custom-c-eldoc" ;; c-eldoc 設定
					"custom-elscreen" ;; elscreen 設定
					"custom-flycheck" ;; flycheck 設定
					"custom-ggtags" ;; ggtags 設定
					"custom-helm" ;; helm 設定
					"custom-migemo" ;; migemo 設定
					"custom-shell-pop" ;; shell-pop 設定
					"custom-smart-compile" ;; smart-compile 設定
					"custom-tramp" ;; TRAMP 設定
					;; "custom-twittering-mode" ;; twittering-mode 設定

					;;-- built-in --;;
					"custom-set-variables"
					"fringe_modeline_buffer"
					"custom-set-faces"
					"add-hook-settings"
					"general-key-bind"
					"print"
					"coding-system"
					"shell-settings"
					"calculate_bootup_time"))
  (unless (load loadfile t)
	(display-loading-error-message loadfile)))

;; ;; esup 確認用
;; ;; -- from packages --;;
;; (load "custom-auto-async-byte-compile") ;; auto-async-byte-compile 設定
;; (load "custom-auto-complete") ;; auto-complete 設定
;; (load "custom-c-eldoc") ;; c-eldoc 設定
;; (load "custom-elscreen") ;; elscreen 設定
;; (load "custom-flycheck") ;; flycheck 設定
;; (load "custom-ggtags") ;; ggtags 設定
;; (load "custom-helm") ;; helm 設定
;; (load "custom-migemo") ;; migemo 設定
;; (load "custom-shell-pop") ;; shell-pop 設定
;; (load "custom-smart-compile") ;; smart-compile 設定
;; (load "custom-tramp") ;; TRAMP 設定
;; (load "custom-twittering-mode") ;; twittering-mode 設定

;; ;;-- built in --;;
;; (load "custom-set-variables")
;; (load "fringe_modeline_buffer")
;; (load "custom-set-faces")
;; (load "add-hook-settings")
;; (load "general-key-bind")
;; (load "print")
;; (load "coding-system")
;; (load "shell-settings")
;; (load "calculate_bootup_time")

