;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; magit
;; git クライアント
;; from package
;;------------------------------------------------------------------------------

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
  (if (string= (car args) "--show-toplevel")
      (substring
	   (shell-command-to-string (format "cygpath -am %s" (apply orig-fun args))) 0 -1)
    (apply orig-fun args)))
(advice-add 'magit-rev-parse :around 'ad-magit-rev-parse|-safe)
(advice-add 'magit-rev-parse-safe :around 'ad-magit-rev-parse|-safe)
