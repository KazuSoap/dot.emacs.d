;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; irony
;; A C/C++ minor mode powered by libclang
;; from package
;;------------------------------------------------------------------------------

;; msys2 で irony-install-server command を動作させる設定
;; https://github.com/Sarcasm/irony-mode/wiki/Setting-up-irony-mode-on-Windows-using-Msys2-and-Mingw-Packages
(defun ad-irony--install-server-read-command (orig-func &rest args)
  "add some option to irony-install-server command for msys2"
  (setenv "CC" "gcc") (setenv "CXX" "g++")
  (defvar irony-cmake-executable)
  (setcar args
		  (replace-regexp-in-string
		   (concat "^\\(.*?" (shell-quote-argument irony-cmake-executable) "\\)")
		   (concat "\\1 -G \"MSYS Makefiles\" -DLIBCLANG_LIBRARY=/mingw64/bin/clang.dll")
		   (car args)))
	(apply orig-func args))
(advice-add 'irony--install-server-read-command :around #'ad-irony--install-server-read-command)

;; (defun ad-irony-cdb-clang-complete--load-db (orig-func &rest args)
;;   "add some option to irony-install-server command for msys2"
;;   (display-object-value "hoge" (apply orig-func args))
;;   )
;; (advice-add 'irony-cdb-clang-complete--load-db :around #'ad-irony-cdb-clang-complete--load-db)

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
   (when (eq system-type 'windows-nt)
	 (setq w32-pipe-read-delay 0)))

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
;; company-irony-c-headers
;; Company Irony C Headers
;; from package
;;------------------------------------------------------------------------------

(defun company-irony-c-headers-hooks ()
  ;;Load with `irony-mode` as a grouped backend
  (defvar company-backends)
  (add-to-list
   'company-backends '(company-irony-c-headers company-irony)))

(with-eval-after-load 'irony
  (autoload 'company-irony-c-headers "company-irony-c-headers" t)
  (dolist (hook '(c-mode-hook c++-mode-hook))
	(add-hook hook 'company-irony-c-headers-hooks)))

;;------------------------------------------------------------------------------
;; flycheck-irony
;; Completion backend for irony-mode
;; from package
;;------------------------------------------------------------------------------

(autoload 'flycheck-irony-setup "flycheck-irony")
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;------------------------------------------------------------------------------
;; irony-eldoc
;; irony-mode support for eldoc-mode
;; from package
;;------------------------------------------------------------------------------

;;(add-hook 'irony-mode-hook 'irony-eldoc)

