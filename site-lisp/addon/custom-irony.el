;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; irony
;; A C/C++ minor mode powered by libclang
;; from package
;;------------------------------------------------------------------------------

;; msys2 で irony を動作させる設定
;; https://github.com/Sarcasm/irony-mode/wiki/Setting-up-irony-mode-on-Windows-using-Msys2-and-Mingw-Packages
(defun ad-irony--install-server-read-command (orig-func &rest args)
  "modify irony--install-server-read-command for msys2"
  (setenv "CC" "gcc") (setenv "CXX" "g++")
  (defvar irony-cmake-executable)
  (setcar args
		  (replace-regexp-in-string
		   (concat "^\\(.*?" (shell-quote-argument irony-cmake-executable) "\\)")
		   (concat "\\1 -G \"MSYS Makefiles\" -DLIBCLANG_LIBRARY=/mingw64/bin/clang.dll")
		   (car args)))
	(apply orig-func args))
(advice-add 'irony--install-server-read-command :around #'ad-irony--install-server-read-command)

;; 追加のコンパイルオプションを設定
(defvar irony-extra-compile-option-alist)
;; clang++ -E -x c++ - -v < /dev/null で確認
(let ((default-inc-path '("-Id:/msys64/mingw64/include/c++/4.9.2"
						  "-Id:/msys64/mingw64/include/c++/4.9.2/x86_64-w64-mingw32"
						  "-Id:/msys64/mingw64/include/c++/4.9.2/backward"
						  "-Id:/msys64/mingw64/lib/clang/3.6.1/include"
						  "-Id:/msys64/mingw64/x86_64-w64-mingw32/include"
						  "-Id:/msys64/mingw64/include")))
  (setq irony-extra-compile-option-alist
		`((c++-mode "-std=c++11" "-lstdc++" ,@default-inc-path)
		  (c-mode ,@(nthcdr 3 default-inc-path)))))

(defun ad-irony--lang-compile-option ()
  "modify cannot load default inc-path on msys2 and cannot apply multiple compile options"
  (defvar irony-lang-compile-option-alist)
  (let ((it (cdr-safe (assq major-mode irony-lang-compile-option-alist))))
	(when it
	  (append `("-x" ,it) (cdr-safe (assq major-mode irony-extra-compile-option-alist))))))
(advice-add 'irony--lang-compile-option :override #'ad-irony--lang-compile-option)

;; 特定のモードで有効化
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
;; <ソース修正>
;; 403: lexical-let -> let
;; 374, 388: remove-if-not -> cl-remove-if-not

(add-hook 'irony-mode-hook 'irony-eldoc)

;; 文字化け対処
(defun ad-irony-eldoc--strip-underscores (string)
  (defvar irony-eldoc-strip-underscores)
  (if (or (not string) (not irony-eldoc-strip-underscores))
      string
    (let ((new-string string)
          (regexps '(("\\_<_+" . ""))))
      (dolist (r regexps)
        (setq new-string
              (replace-regexp-in-string (car r) (cdr r) new-string)))
      new-string)))
(advice-add 'irony-eldoc--strip-underscores :override #'ad-irony-eldoc--strip-underscores)
