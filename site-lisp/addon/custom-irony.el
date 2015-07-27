;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; irony
;; A C/C++ minor mode powered by libclang
;; from package
;;------------------------------------------------------------------------------

;; msys2 で irony-install-server command を動作させる設定
(defun ad-irony--install-server-read-command (orig-func &rest args)
  "add some option to irony-install-server command for msys2"
  (setenv "CC" "gcc") (setenv "CXX" "g++")
  (setcar args
		  (replace-regexp-in-string
		   "^\\(.*?cmake\\)"
		   "\\1 -G \"MSYS Makefiles\" -DLIBCLANG_LIBRARY=/mingw64/bin/clang.dll"
		   (car args)))
  (apply orig-func args))
(advice-add 'irony--install-server-read-command :around #'ad-irony--install-server-read-command)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (defvar irony-mode-map)
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
	'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(with-eval-after-load 'irony
  (setq w32-pipe-read-delay 0))

;;------------------------------------------------------------------------------
;; company-irony
;; Completion backend for irony-mode
;; from package
;;------------------------------------------------------------------------------

(with-eval-after-load 'company
  (defvar company-backends)
  (add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;------------------------------------------------------------------------------
;; ac-irony
;; Auto-complete support for irony-mode
;; from https://github.com/Sarcasm/ac-irony
;;------------------------------------------------------------------------------

;;(setenv "CC" "clang")
;;(setenv "CXX" "clang++")

;; (require 'ac-irony)

;; (defun my-ac-irony-setup ()
;;   (add-to-list 'ac-sources 'ac-source-irony)
;;   ;;(auto-complete-mode 1)
;;   (define-key irony-mode-map (kbd "C-;") 'ac-complete-irony-async))

;; (add-hook 'irony-mode-hook 'my-ac-irony-setup)

;; (defun my-ac-irony-setup ()
;;   ;; be cautious, if yas is not enabled before (auto-complete-mode 1), overlays
;;   ;; *may* persist after an expansion.
;; ;  (yas-minor-mode 1)
;;   (auto-complete-mode 1)

;;   (defvar ac-sources)
;;   (add-to-list 'ac-sources 'ac-source-irony)

;;   (defvar irony-mode-map)
;;   (define-key irony-mode-map (kbd "M-RET") 'ac-complete-irony-async))

;; (add-hook 'irony-mode-hook 'my-ac-irony-setup)
