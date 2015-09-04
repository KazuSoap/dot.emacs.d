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
(dolist (path '("~/.emacs.d/site-lisp"))
  (let((default-directory (expand-file-name path)))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
		(normal-top-level-add-subdirs-to-load-path))))

;;------------------------------------------------------------------------------
;; local functions & macro
;;------------------------------------------------------------------------------
(defun display-object-value (bufer_name &rest args)
  "Display Object Value"
  (let ((buffer (get-buffer-create bufer_name)))
	(set-buffer buffer)
	(dolist (i args)
	  (insert (format "%s" i)))
	(display-buffer (current-buffer))))

(defun autoload-if-found (function file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (and (locate-library file)
       (autoload function file docstring interactive type)))

(when (eq system-type 'windows-nt)
  (defun cygpath (option path)
	"cygpath for emacs lisp"
	(let ((command (mapconcat 'identity (list "D:/msys64/usr/bin/cygpath"
											   option
											   (shell-quote-argument path)) "\s")))
	  (substring (shell-command-to-string command) 0 -1))))

(defun message-startup-time ()
  "echo bootup time in message buffer"
  (message "Emacs loaded in %d ms"
		   (* 1000 (float-time (time-subtract after-init-time before-init-time)))))
(add-hook 'after-init-hook 'message-startup-time)

(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
	  (revert-buffer :ignore-auto :noconfirm)
	(error "The buffer has been modified")))

;;------------------------------------------------------------------------------
;; load files
;;------------------------------------------------------------------------------
;; -- addon --;;
(load "custom-auto-async-byte-compile") ;; auto-async-byte-compile 設定
(load "custom-company") ;; company 設定
(load "custom-elscreen") ;; elscreen 設定
(load "custom-flycheck") ;; flycheck 設定
(load "custom-ggtags") ;; ggtags 設定
(load "custom-helm") ;; helm 設定
(load "custom-irony") ;; irony 設定
(load "custom-migemo") ;; migemo 設定
(load "custom-shell-pop") ;; shell-pop 設定
(load "custom-smart-compile") ;; smart-compile 設定
(load "custom-twittering-mode") ;; twittering-mode 設定

;;-- built in --;;
(load "add-hook-settings")
(load "custom-aute-insert")
(load "custom-gdb")
(load "custom-tramp") ;; TRAMP 設定
(load "fringe_modeline_buffer")
(load "general-key-bind")
(load "print")
(load "shell-settings")
(load "custom-set-variables")

