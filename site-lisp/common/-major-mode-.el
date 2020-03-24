;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; major-mode
;;------------------------------------------------------------------------------
;; 派生モード elisp-mode
(define-derived-mode elisp-mode emacs-lisp-mode "ELisp")

;; 拡張子による major-mode の関連付け
(add-to-list 'auto-mode-alist '("\\.el$" . elisp-mode))
(add-to-list 'auto-mode-alist '("\\.elc$" . fundamental-mode))

;;------------------------------------------------------------------------------
;; major-mode-hook
;;------------------------------------------------------------------------------
;; 共通
(eval-when-compile
  (defsubst my-common-mode-setup ()
    ;; (cua-mode) ;; cua
    (display-line-numbers-mode)
    (whitespace-mode))) ;; whitespace

;; プログラミング言語共通
(eval-when-compile
  (defsubst my-common-programing-mode-setup ()
    (setq tab-width 4) ;; tab 幅
    (setq truncate-lines t) ;; 画面外文字の切り詰め
    (setq truncate-partial-width-windows t) ;; 縦分割時の画面外文字の切り詰め
    (show-paren-mode) ;; 括弧のハイライト
    (company-mode))) ;; 補完

;; text-mode
(fset 'my-text-mode-setup
      (lambda ()
        (my-common-mode-setup)))
(add-hook 'text-mode-hook 'my-text-mode-setup)

;; c-mode-common
(fset 'my-c-mode-common-setup
      (lambda ()
        (my-common-programing-mode-setup)
        (my-common-mode-setup)))
(add-hook 'c-mode-common-hook 'my-c-mode-common-setup)

;; c/c++-mode
(eval-when-compile
  (defsubst my-c/c++-mode-setup ()
    ;; (eldoc-mode)
    (flycheck-mode)
    (flycheck-irony-setup)
    (ggtags-mode)
    (irony-mode)
    (google-set-c-style)
    (google-make-newline-indent)
    (add-to-list (make-local-variable 'company-backends) '(company-irony-c-headers company-irony))))

(fset 'my-c-mode-setup
      (lambda ()
        (my-c/c++-mode-setup)
        ;; irony 追加のコンパイルオプションを設定
        (setq irony-additional-clang-options '("-std=c99"))))
(add-hook 'c-mode-hook 'my-c-mode-setup)

(fset 'my-c++-mode-setup
      (lambda ()
        (my-c/c++-mode-setup)
        ;; irony 追加のコンパイルオプションを設定
        (setq irony-additional-clang-options '("-std=c++14"))))
(add-hook 'c++-mode-hook 'my-c++-mode-setup)

;; emacs-lisp-mode
(fset 'my-emacs-lisp-mode-setup
      (lambda () (setq lexical-binding t)))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-setup)

;; elisp-mode
(fset 'my-elisp-mode-setup
      (lambda ()
        (my-common-programing-mode-setup)
        (my-common-mode-setup)
        (flycheck-mode)))
(add-hook 'elisp-mode-hook 'my-elisp-mode-setup)

;; ;; plantuml-mode
;; (fset 'my-plantuml-mode-setup
;;       (lambda ()
;;         (my-common-programing-mode-setup)
;;         (my-common-mode-setup)
;;         (flycheck-mode)
;;         (flycheck-plantuml-setup)))
;; (add-hook 'plantuml-mode-hook 'my-plantuml-mode-setup)

;; sh-mode
(fset 'my-sh-mode-setup
      (lambda ()
        (my-common-programing-mode-setup)
        (my-common-mode-setup)
        (flycheck-mode)))
(add-hook 'sh-mode-hook 'my-sh-mode-setup)

;; javascript mode
(fset 'my-js-mode-setup
      (lambda ()
        (my-common-programing-mode-setup)
        (my-common-mode-setup)))
(add-hook 'js-mode-hook 'my-js-mode-setup)

;; python-mode
(fset 'my-python-mode-setup
      (lambda ()
        (my-common-programing-mode-setup)
        (my-common-mode-setup)
        (ggtags-mode)
        (highlight-indent-guides-mode)
        (setq-default python-indent-offset 2)))
(add-hook 'python-mode-hook 'my-python-mode-setup)
