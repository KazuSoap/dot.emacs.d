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
  (defvar irony-cmake-executable)
  (setcar args
		  (replace-regexp-in-string
		   (concat "^\\(.*?" (shell-quote-argument irony-cmake-executable) "\\)")
		   (concat "\\1 -G \"MSYS Makefiles\" -DLIBCLANG_LIBRARY=/mingw64/bin/clang.dll")
		   (car args)))
	(apply orig-func args))
(advice-add 'irony--install-server-read-command :around #'ad-irony--install-server-read-command)

(setenv "LIBCLANG_LIBRARY" "/mingw64/bin/clang.dll")

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

;; (defvar irony-lang-compile-option-alist)
;; (setq irony-lang-compile-option-alist
;;       '((c++-mode . ("c++" "-std=c++11" "-lstdc++" "-lm"))
;;         (c-mode . ("c"))
;;         (objc-mode . '("objective-c"))))
;; (defun irony--lang-compile-option ()
;;   (irony--awhen (cdr-safe (assq major-mode irony-lang-compile-option-alist))
;;     (append '("-x") it)))

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

(with-eval-after-load 'company
  (autoload 'company-irony-c-headers "company-irony-c-headers" t)
  (dolist (hook '(c-mode-hook c++-mode-hook))
	(add-hook hook 'company-irony-c-headers-hooks)))

;;------------------------------------------------------------------------------
;; flycheck-irony
;; Completion backend for irony-mode
;; from package
;;------------------------------------------------------------------------------

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
