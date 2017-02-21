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
(defvar add-dir-list)
(setq add-dir-list '("site-lisp/common" "extra_modules"))

(cond ((eq system-type 'windows-nt)
       (add-to-list 'add-dir-list "site-lisp/win"))
      ((eq system-type 'gnu/linux)
       (add-to-list 'add-dir-list "site-lisp/linux")))

(dolist (dir add-dir-list)
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
(load "-irony-")
(load "-migemo-")
(load "-twittering-mode-")
(load "-add-hook-")
(load "-aute-insert-")
(load "-gdb-")
(load "-fringe_modeline_buffer-")
(load "-general-key-bind-")
(load "-shell-")
(load (setq custom-file "-custom-set-variables-"))
;; (with-eval-after-load 'kkc
;;   (load "-kkc-cmd-")
;;   (load "-kkc-popup-"))
