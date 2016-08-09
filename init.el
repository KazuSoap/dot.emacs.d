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
(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/") t)

;;------------------------------------------------------------------------------
;; load path
;;------------------------------------------------------------------------------
(dolist (dir '("site-lisp" "extra_modules"))
  (let((default-directory (expand-file-name(concat user-emacs-directory dir))))
    (add-to-list 'load-path default-directory)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

;;------------------------------------------------------------------------------
;; local functions & macro
;;------------------------------------------------------------------------------
(defun message-startup-time ()
  "echo bootup time in message buffer"
  (message "Emacs loaded in %d ms"
           (* 1000 (float-time (time-subtract after-init-time before-init-time)))))
(add-hook 'after-init-hook 'message-startup-time)

(when (eq system-type 'windows-nt)
  (defun cygpath (&optional option path)
    "cygpath for emacs lisp"
    (if path (with-temp-buffer
               (call-process "d:/msys64/usr/bin/cygpath" nil '(t nil) nil option path)
               (unless (bobp)
                 (goto-char (point-min))
                 (buffer-substring-no-properties (point) (line-end-position)))))))

(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))

(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((dx (if (= (nth 0 (window-edges)) 0) 1 -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
        action c)
    (message "%s" action)
    (catch 'end-flag
      (while t
        (setq action (read-key-sequence-vector (format "size[%dx%d]" (window-width) (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l) (enlarge-window-horizontally dx))
              ((= c ?h) (shrink-window-horizontally dx))
              ((= c ?j) (enlarge-window dy))
              ((= c ?k) (shrink-window dy))
              (t (let ((command (key-binding action)))
                   (when command (call-interactively command)))
                 (message "Quit")
                 (throw 'end-flag t)))))))

;;------------------------------------------------------------------------------
;; load files
;;------------------------------------------------------------------------------
;; -- addon --;;
(load "custom-auto-async-byte-compile")
(load "custom-company")
(load "custom-elscreen")
(when (eq system-type 'windows-nt)
  (load "custom-fakecygpty"))
(load "custom-flycheck")
(load "custom-ggtags")
(load "custom-helm")
(load "custom-irony")
(load "custom-magit")
(with-eval-after-load "general-key-bind"
  (load "custom-migemo"))
(load "custom-shell-pop")
(load "custom-twittering-mode")

;; -- built in -- ;;
(load "add-hook-settings")
(load "custom-aute-insert")
(load "custom-gdb")
(load "custom-tramp")
(load "fringe_modeline_buffer")
(load "general-key-bind")
(load "print")
(load "shell-settings")
(load "custom-set-variables")
(with-eval-after-load 'kkc
  (load "kkc-cmd")
  (load "kkc-popup"))
