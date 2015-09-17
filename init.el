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
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
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
(defun autoload-if-found (function file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (and (locate-library file)
       (autoload function file docstring interactive type)))

(when (eq system-type 'windows-nt)
  (defun cygpath (&optional option path)
	"cygpath for emacs lisp"
	(if path
		(with-temp-buffer
		  (process-file "d:/msys64/usr/bin/cygpath" nil '(t nil) nil option path)
		  (unless (bobp)
			(goto-char (point-min))
			(buffer-substring-no-properties (point) (line-end-position)))))))

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
(load "custom-auto-async-byte-compile")
(load "custom-company")
(load "custom-elscreen")
(load "custom-fakecygpty")
(load "custom-flycheck")
(load "custom-ggtags")
(load "custom-helm")
(load "custom-irony")
(load "custom-magit")
(load "custom-migemo")
(load "custom-shell-pop")
(load "custom-smart-compile")
(load "custom-twittering-mode")

;;-- built in --;;
(load "add-hook-settings")
(load "custom-aute-insert")
(load "custom-gdb")
(load "custom-tramp")
(load "fringe_modeline_buffer")
(load "general-key-bind")
(load "print")
(load "shell-settings")
(load "custom-set-variables")
