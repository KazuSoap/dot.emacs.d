;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; migemo
;; ローマ字入力で日本語文字列を検索
;; from : package system
;;------------------------------------------------------------------------------

(cond ((require 'migemo nil t)
	   (defvar migemo-command)
	   (defvar migemo-dictionary)
	   (cond ((eq system-type 'windows-nt)
			  (setq migemo-command "d:/Program Files/cmigemo/cmigemo")
			  (setq migemo-dictionary (expand-file-name "d:/Program Files/cmigemo/dict/utf-8/migemo-dict")))
			 ((eq system-type 'gnu/linux)
			  (setq migemo-command "cmigemo")
			  (setq migemo-dictionary (expand-file-name "/usr/share/cmigemo/utf-8/migemo-dict"))))

	   (defvar migemo-options)
	   (setq migemo-options '("-q" "--emacs"))

	   (defvar migemo-user-dictionary)
	   (setq migemo-user-dictionary nil)

	   (defvar migemo-regex-dictionary)
	   (setq migemo-regex-dictionary nil)

	   (defvar migemo-coding-system)
	   (setq migemo-coding-system 'utf-8-unix)

	   (load-library "migemo")
	   (migemo-init)

	   ;; local key-bind
	   (define-key isearch-mode-map (kbd "C-M-y") 'migemo-isearch-yank-char)
	   (define-key isearch-mode-map (kbd "C-w") 'migemo-isearch-yank-word)
	   (define-key isearch-mode-map (kbd "M-s C-e") 'migemo-isearch-yank-line)
	   (define-key isearch-mode-map (kbd "M-m") 'migemo-isearch-toggle-migemo)
	   (define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill))
	  (t (display-loading-error-message "migemo")))

;;------------------------------------------------------------------------------
;; helm-migemo
;; helmでmigemo検索
;; from : package system
;;------------------------------------------------------------------------------

;; helm で正しく migemo を動作させるための対策
;; http://rubikitch.com/2014/12/19/helm-migemo/
;; https://github.com/emacs-helm/helm/pull/379

(cond ((autoload-if-found 'helm-aif "helm-lib" t)
	   (autoload 'helm-candidates-in-buffer "helm" t)
	   (autoload 'helm-get-current-source "helm" t)

	   (defun helm-compile-source--candidates-in-buffer (source)
		 (let ((test-form (assoc 'candidates-in-buffer source)))
		   (helm-aif test-form
			   (append source
					   `((candidates
						  . ,(or (cdr test-form)
								 (lambda ()
								   ;; Do not use `source' because other plugins
								   ;; (such as helm-migemo) may change it
								   (helm-candidates-in-buffer (helm-get-current-source)))))
						 (volatile) (match identity)))
			 source))))
	  (t (display-loading-error-message "helm-lib")))

(with-eval-after-load 'helm-lib
  (unless (require 'helm-migemo nil t)
	(display-loading-error-message "helm-migemo")))

