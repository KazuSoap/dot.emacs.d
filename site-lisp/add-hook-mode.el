;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; add-hook-mode
;;------------------------------------------------------------------------------
;;-- fileの関連付け --;;
;(setq auto-mode-alist (append (list
;                               '("\\.txt$" . text-mode)
;                              auto-mode-alist)))

;;-- 不要なhookを外す --;;
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)

;;-- mode設定 --;;
;;共通設定
(dolist (hook '(text-mode-hook
				emacs-lisp-mode-hook
				sh-mode-hook))
  (add-hook hook '(lambda () (setq tab-width 4))));;タブ幅4

;;text-mode
(add-hook 'text-mode-hook '(lambda ()
	(font-lock-mode t)

	;; (defface sub-subSect-face '((t (:background "black" :foreground "green"))) nil)
	;; (defface subSect-face '((t (:background "black" :foreground "#a2d29e"))) nil)
	;; (defface Sect-face '((t (:background "black" :foreground "gold"))) nil)
	;; (defface para-face '((t (:foreground "violet"))) nil)
	;; (defface 0-9-face '((t (:foreground "white"))) nil)
	;; (defface A-z-face '((t (:background "black" :foreground "#c0d136" ))) nil)
	;; (defface .-face '((t (:background "#343d55" :foreground "black" ))) nil)
	;; (defface yaku-face '((t (:foreground "#FF3333"))) nil)

	;; (defvar sub-subSect-face 'sub-subSect-face)
	;; (defvar subSect-face 'subSect-face)
	;; (defvar Sect-face 'Sect-face)
	;; (defvar para-face 'para-face)
	;; (defvar 0-9-face '0-9-face)
	;; (defvar A-z-face 'A-z-face)
	;; (defvar .-face '.-face)
	;; (defvar yaku-face 'yaku-face)

	;; (defadvice font-lock-mode (before text-font-lock-mode ())
	;;   (font-lock-add-keywords major-mode '(
	;; 	( "\\(/\\*sub-.*\\)+\\(\n\\++\\)*" 0 sub-subSect-face append)
	;; 	( "\\(\\+\\++\\)*" 0 sub-subSect-face append)
	;; 	( "\\(/\\*s.*\\)+\\(\n~+\\)*" 0 subSect-face append)
	;; 	( "\\(~~+\\)*" 0 subSect-face append)
	;; 	( "\\(/\\*.*\\)+\\(\n\\*+\\)*" 0 Sect-face append)
	;; 	( "\\(\\*\\*+\\)*" 0 Sect-face append)
	;; 	( "\\(---+\\)*\\(|<.*\\)*" 0 para-face append)
	;; 	( "[0-9]" 0 0-9-face append)
	;; 	( "[a-z]*[A-Z]*" 0 A-z-face append)
	;; 	( "\\." 0 .-face append)
	;; 	( "||.*" 0 yaku-face append))))
	;; (ad-enable-advice 'font-lock-mode 'before 'text-font-lock-mode)
	;; (ad-activate 'font-lock-mode)
	))

;;emacs-lisp-mode
;;(add-hook 'emacs-lisp-mode-hook '(lambda () (setq tab-width 4)))

;;sh-mode
;;(add-hook 'sh-mode-hook '(lambda () (setq tab-width 4)))

;;c,c++mode
(autoload 'vs-set-c-style "vs-set-c-style")
(dolist (hook '(c-mode-hook
				c++-mode-hook))
  (add-hook hook '(lambda ()
					(auto-complete-mode t)
					(vs-set-c-style))))

