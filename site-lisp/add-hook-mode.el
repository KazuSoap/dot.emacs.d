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
	))

;;c,c++mode
(autoload 'vs-set-c-style "vs-set-c-style")
(dolist (hook '(c-mode-hook
				c++-mode-hook))
  (add-hook hook '(lambda ()
					(auto-complete-mode t)
					(vs-set-c-style))))

