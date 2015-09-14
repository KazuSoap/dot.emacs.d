;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; magit
;; git クライアント
;; from package
;;------------------------------------------------------------------------------

(defun ad-magit-read-repository (orig-fun &rest args)
  (let ((ret_val (apply orig-fun args)))
	(if ret_val (file-truename ret_val))))
(advice-add 'magit-read-repository :around 'ad-magit-read-repository)

(autoload 'magit-process-git-arguments "magit-git")
(defun ad-magit-process-git-arguments (orig-fun &rest args)
  (mapcar
   (lambda (x) (replace-regexp-in-string "format=\\(.*\\)" "format='\\1'" x))
   (apply orig-fun args)))
(advice-add 'magit-process-git-arguments :around 'ad-magit-process-git-arguments)

(defvar magit-git-executable)
(defun ad-magit-git-insert (&rest args)
  (setq args (magit-process-git-arguments args))
  (apply #'process-file-shell-command magit-git-executable nil (list t nil) nil args))
(advice-add 'magit-git-insert :override 'ad-magit-git-insert)

(defun ad-magit-rev-parse|-safe (orig-fun &rest args)
  (let ((ret_val (apply orig-fun args)))
	(if (and (string= (car args) "--show-toplevel") ret_val)
		(substring (shell-command-to-string (format "cygpath -am %s" ret_val)) 0 -1)
	  ret_val)))
(advice-add 'magit-rev-parse :around 'ad-magit-rev-parse|-safe)
(advice-add 'magit-rev-parse-safe :around 'ad-magit-rev-parse|-safe)
