;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; site-lisp 設定
;;------------------------------------------------------------------------------
(dolist (loadfile '("custom-elscreen" ;; elscreen 設定
					"custom-auto-complete" ;; auto-complete 設定
					"custom-flycheck" ;; flycheck 設定
					"custom-tramp" ;; TRAMP 設定
					"custom-migemo" ;; migemo 設定
					"custom-helm" ;; helm 設定
					"custom-auto-async-byte-compile" ;; auto-async-byte-compile 設定
					"custom-sticky" ;; sticky shift 設定
					"custom-shell-pop" ;; shell-pop 設定
					;;"custom-pop_window" ;; pop_window 設定
					));; "custom-twittering-mode")) ;; twittering-mode 設定
  (unless (load loadfile t)
	(display-loading-error-message loadfile)))
