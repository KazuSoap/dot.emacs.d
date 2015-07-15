;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; add-hook-mode
;;------------------------------------------------------------------------------
;;-- デフォルトのメジャーモード --;;
(setq-default major-mode 'text-mode)
;
;;-- fileの関連付け --;;
;(setq auto-mode-alist (append (list
;                               '("\\.txt$" . text-mode)
;                              auto-mode-alist)))

;;-- 不要なhookを外す --;;
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)

;;-- mode設定 --;;
;;共通設定 (有効化)
(dolist (hook '(text-mode-hook emacs-lisp-mode-hook
				sh-mode-hook makefile-mode-hook
				c-mode-hook c++-mode-hook))
  (add-hook hook '(lambda ()
					(setq tab-width 4)
					(whitespace-mode t)
					(linum-mode 1)
					(cua-mode t)
					(show-paren-mode t))))

;;共通設定 (無効化)
(dolist (hook '(esup-mode-hook lisp-interaction-mode-hook
				emacs-lisp-byte-code-mode-hook))
  (add-hook hook '(lambda ()
					(whitespace-mode 0)
					(linum-mode 0)
					(cua-mode 0)
					(show-paren-mode 0))))

;;text-mode
(add-hook 'text-mode-hook '(lambda ()
							 (font-lock-mode t)))

;;c,c++mode
(autoload 'vs-set-c-style "vs-set-c-style")
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook '(lambda ()
					(vs-set-c-style))))

