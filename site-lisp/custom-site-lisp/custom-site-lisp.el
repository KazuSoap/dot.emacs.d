;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; site-lisp 設定
;;------------------------------------------------------------------------------
(dolist (loadfile '("custom-auto-async-byte-compile" ;; auto-async-byte-compile 設定
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
					));; "custom-twittering-mode")) ;; twittering-mode 設定
  (unless (load loadfile t)
	(display-loading-error-message loadfile)))

;; esup 確認用
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
