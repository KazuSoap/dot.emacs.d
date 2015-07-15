;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; migemo 設定
;;------------------------------------------------------------------------------
;;; migemo ---------------------------------------------------------------------
;; ローマ字入力で日本語文字列を検索
;; from : package system

(require 'migemo)

(setq migemo-command "d:/Program Files/cmigemo/cmigemo")
(setq migemo-options '("-q" "--emacs"))

;; Set your installed path
(setq migemo-dictionary (expand-file-name "d:/Program Files/cmigemo/dict/utf-8/migemo-dict"))
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)

(load-library "migemo")
(migemo-init)

;; local key-bind
(define-key isearch-mode-map (kbd "C-M-y") 'migemo-isearch-yank-char)
(define-key isearch-mode-map (kbd "C-w") 'migemo-isearch-yank-word)
(define-key isearch-mode-map (kbd "M-s C-e") 'migemo-isearch-yank-line)
(define-key isearch-mode-map (kbd "M-m") 'migemo-isearch-toggle-migemo)
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)

;;; helm-migemo ----------------------------------------------------------------
;; helmでmigemo検索
;; from : package system

(with-eval-after-load 'helm-lib
  (require 'helm-migemo)

  ;; helm で正しく migemo を動作させるための対策
  ;; http://rubikitch.com/2014/12/19/helm-migemo/
  ;; https://github.com/emacs-helm/helm/pull/379
  (defun helm-compile-source--candidates-in-buffer (source)
	(helm-aif (assoc 'candidates-in-buffer source)
		(append source
				`((candidates
				   . ,(or (cdr it)
						  (lambda ()
							;; Do not use `source' because other plugins
							;; (such as helm-migemo) may change it
							(helm-candidates-in-buffer (helm-get-current-source))))
				   )
				  (volatile) (match identity)
				  )
				)source)
	)
  )
