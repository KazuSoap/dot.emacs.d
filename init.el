;;; init.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; from package
;;==============================================================================
;;------------------------------------------------------------------------------
;; cygwin-mount
;; teach EMACS about cygwin styles and mount points
;; https://www.emacswiki.org/emacs/cygwin-mount.el
;;------------------------------------------------------------------------------
;; fakecygpty
;; cygwin's pty feature for NTEmacs
;; https://github.com/d5884/fakecygpty
;;------------------------------------------------------------------------------
(with-eval-after-load 'shell
  (require 'my-package))

;;------------------------------------------------------------------------------
;; IME
;;------------------------------------------------------------------------------
(eval-when-compile
  (defmacro set-my-ime ()
    (cond ((eq system-type 'windows-nt)
           (declare-function tr-ime-hook-check "tr-ime-hook")

           `(progn
              ;; 無変換キーで tr-ime & w32-ime を有効化
              (global-set-key (kbd "<non-convert>")
                              (lambda ()
                                (interactive)
                                (global-unset-key (kbd "<non-convert>"))

                                ;; tr-imeの有効化
                                ;; (tr-ime-advanced-install)
                                (tr-ime-advanced-initialize)
                                (tr-ime-hook-check)

                                (require 'my-package)))))

          ((eq system-type 'gnu/linux)
           `(progn
              (setq default-input-method "japanese-mozc")
              (with-eval-after-load 'mozc
                (require 'my-package))))
          )))
(set-my-ime)

;;------------------------------------------------------------------------------
;; helm
;; an Emacs incremental and narrowing framework
;;------------------------------------------------------------------------------
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-r") #'helm-recentf)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "C-c i") #'helm-imenu)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)

;;------------------------------------------------------------------------------
;; migemo
;; Japanese incremental search through dynamic pattern expansion
;;------------------------------------------------------------------------------
(add-hook 'isearch-mode-hook (lambda () (require 'shell) (require 'cmigemo)))

;;==============================================================================
;; major-mode
;;==============================================================================
;;------------------------------------------------------------------------------
;; auto-mode-alist
;;------------------------------------------------------------------------------
(eval-when-compile
  (require 'cc-mode)
  (require 'flycheck)
  (require 'python)
  (require 'web-mode)
  ;; (require 'plantuml-mode)
  )

;; 派生モード elisp-mode
;; (define-derived-mode elisp-mode emacs-lisp-mode "ELisp")

;; 拡張子による major-mode の関連付け
;; (add-to-list 'auto-mode-alist '("\\.el\\'" . elisp-mode))
(add-to-list 'auto-mode-alist '("\\.elc\\'" . fundamental-mode))
(add-to-list 'auto-mode-alist '("\\.\\(html?\\|ptml?\\|php?\\|tpl?\\|js?\\|vue?\\)\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.puml?\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;;------------------------------------------------------------------------------
;; major-mode-hook
;;------------------------------------------------------------------------------
(eval-when-compile
  (require 'cc-mode)
  (require 'flycheck)
  (require 'python)
  (require 'web-mode)
  ;; (require 'plantuml-mode)

  ;; 共通
  (defsubst my-common-mode-setup ()
    (require 'my-package)
    (display-line-numbers-mode)
    (whitespace-mode)) ;; whitespace

  ;; プログラミング言語共通
  (defsubst my-common-programing-mode-setup ()
    (setq tab-width 4) ;; tab 幅
    (setq truncate-lines t) ;; 画面外文字の切り詰め
    (setq truncate-partial-width-windows t) ;; 縦分割時の画面外文字の切り詰め
    ;; (show-paren-mode) ; default on
    (company-mode)) ;; 補完

  ;; c/c++-mode共通
  (defsubst my-c/c++-mode-setup ()
    ;; (eldoc-mode)
    (flycheck-mode)
    (flycheck-irony-setup)
    (ggtags-mode)
    (irony-mode)
    (google-set-c-style)
    (google-make-newline-indent)
    (setq c-basic-offset 4)
    (add-to-list (make-local-variable 'company-backends) '(company-irony-c-headers company-irony)))
  )

;; text-mode
(fset 'my-text-mode-setup
      (lambda ()
        (my-common-mode-setup)))
(add-hook 'text-mode-hook 'my-text-mode-setup)

;; emacs-lisp-mode / elisp-mode
(fset 'my-elisp-mode-setup
      (lambda ()
        (my-common-programing-mode-setup)
        (my-common-mode-setup)
        (flycheck-mode)))
;; (add-hook 'elisp-mode-hook 'my-elisp-mode-setup)
(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-setup)

;; c-mode-common
(fset 'my-c-mode-common-setup
      (lambda ()
        (my-common-programing-mode-setup)
        (my-common-mode-setup)))
(add-hook 'c-mode-common-hook 'my-c-mode-common-setup)

;; c-mode
(fset 'my-c-mode-setup
      (lambda ()
        (my-c/c++-mode-setup)
        ;; irony 追加のコンパイルオプションを設定
        (setq irony-additional-clang-options '("-std=c99"))))
(add-hook 'c-mode-hook 'my-c-mode-setup)

;; c++-mode
(fset 'my-c++-mode-setup
      (lambda ()
        (my-c/c++-mode-setup)
        ;; irony 追加のコンパイルオプションを設定
        (setq irony-additional-clang-options '("-std=c++14"))))
(add-hook 'c++-mode-hook 'my-c++-mode-setup)

;; ;; plantuml-mode
;; (fset 'my-plantuml-mode-setup
;;       (lambda ()
;;         (my-common-programing-mode-setup)
;;         (my-common-mode-setup)
;;         (flycheck-mode)
;;         (flycheck-plantuml-setup)
;;         (setq plantuml-indent-level 2)))
;; (add-hook 'plantuml-mode-hook 'my-plantuml-mode-setup)

;; sh-mode
(fset 'my-sh-mode-setup
      (lambda ()
        (my-common-programing-mode-setup)
        (my-common-mode-setup)
        (flycheck-mode)))
(add-hook 'sh-mode-hook 'my-sh-mode-setup)

;; ;; javascript mode
;; (fset 'my-js-mode-setup
;;       (lambda ()
;;         (my-common-programing-mode-setup)
;;         (my-common-mode-setup)))
;; (add-hook 'js-mode-hook 'my-js-mode-setup)

;; web mode
(fset 'my-web-mode-setup
      (lambda ()
        (my-common-mode-setup)
        (highlight-indent-guides-mode)
        (company-mode)
        (setq truncate-lines t) ;; 画面外文字の切り詰め
        (setq truncate-partial-width-windows t) ;; 縦分割時の画面外文字の切り詰め

        (setq web-mode-markup-indent-offset 2)
        (setq web-mode-css-indent-offset 2)
        (setq web-mode-code-indent-offset 2)
        ))
(add-hook 'web-mode-hook 'my-web-mode-setup)

;; python-mode
(fset 'my-python-mode-setup
      (lambda ()
        (my-common-programing-mode-setup)
        (my-common-mode-setup)
        ;; (ggtags-mode)
        ;; (setq flycheck-disabled-checkers '(python-mypy))
        (highlight-indent-guides-mode)
        (setq python-indent-offset 4)))
(add-hook 'python-mode-hook 'my-python-mode-setup)

;;; init.el ends here
