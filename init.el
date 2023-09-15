;;; init.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; from package
;;==============================================================================
;;------------------------------------------------------------------------------
;; cygwin-mount / fakecygpty
;;------------------------------------------------------------------------------
(with-eval-after-load 'shell
  (require 'my-package))

;;------------------------------------------------------------------------------
;; helm
;;------------------------------------------------------------------------------
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-r") #'helm-recentf)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "C-c i") #'helm-imenu)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)

;;------------------------------------------------------------------------------
;; IME
;;------------------------------------------------------------------------------
(eval-when-compile
  (defmacro set-my-ime ()
    (cond ((eq system-type 'windows-nt)
           (declare-function tr-ime-hook-check "tr-ime-hook")

           `(progn
              ;; Enable tr-ime & w32-ime with [non-convert] key
              (global-set-key (kbd "<non-convert>")
                              (lambda ()
                                (interactive)
                                (global-unset-key (kbd "<non-convert>"))

                                ;; (tr-ime-advanced-install)
                                (tr-ime-advanced-initialize)
                                (tr-ime-hook-check)

                                (require 'my-package)))))

          ((eq system-type 'gnu/linux)
           `(progn
              (setq default-input-method "japanese-mozc")
              (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
              (global-set-key (kbd "<henkan>") (lambda () (interactive) (activate-input-method default-input-method)))
              (global-set-key (kbd "<hiragana-katakana>") (lambda () (interactive) (activate-input-method default-input-method)))
              (global-set-key (kbd "<muhenkan>") (lambda () (interactive) (deactivate-input-method)))

              (with-eval-after-load 'mozc
                (require 'my-package))))
          )))
(set-my-ime)

;;------------------------------------------------------------------------------
;; migemo
;;------------------------------------------------------------------------------
(add-hook 'isearch-mode-hook (lambda () (require 'shell) (require 'cmigemo)))

;;==============================================================================
;; major-mode
;;==============================================================================
;;------------------------------------------------------------------------------
;; major-mode-hook
;;------------------------------------------------------------------------------
(eval-when-compile
  (require 'cc-mode)
  (require 'markdown-mode)
  (require 'plantuml-mode)
  (require 'web-mode))

(let* (;; common setup
       (my-mode-setup
        (lambda ()
          (require 'my-package)
          (display-line-numbers-mode)
          (whitespace-mode)
          ))

       ;; common setup for programming languages
       (my-prog-mode-setup
        (lambda (tab-w)
          (funcall my-mode-setup)

          (setq tab-width tab-w)

          ;; Truncate off-screen characters
          (setq truncate-lines t) ;; default
          (setq truncate-partial-width-windows t) ;; when vertical split screen

          ;; (show-paren-mode) ; default on
          (company-mode)
          ))

       ;; common setup for c/c++-mode
       (my-c/c++-mode-setup
        (lambda (irony-option)
          ;; (eldoc-mode)
          (flycheck-mode)
          (flycheck-irony-setup)
          (ggtags-mode)
          (irony-mode)
          (google-set-c-style)
          (google-make-newline-indent)
          (setq c-basic-offset 4)
          (add-to-list (make-local-variable 'company-backends) '(company-irony))

          ;; irony > additional compile options
          (setq irony-additional-clang-options irony-option)
          )))

  ;; built-in
  (add-hook 'c-mode-common-hook (apply-partially my-prog-mode-setup 4))

  (add-hook 'c-mode-hook (apply-partially my-c/c++-mode-setup '("-std=c99")))

  (add-hook 'c++-mode-hook (apply-partially my-c/c++-mode-setup '("-std=c++14")))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (funcall my-prog-mode-setup 4)
              (flycheck-mode)
              ))

  (add-hook 'sh-mode-hook
            (lambda ()
              (funcall my-prog-mode-setup 4)
              (flycheck-mode)
              ))

  (add-hook 'python-mode-hook
            (lambda ()
              (funcall my-prog-mode-setup 4)
              (highlight-indent-guides-mode)
              (flycheck-mode)
              ;; (ggtags-mode)
              ;; (setq flycheck-disabled-checkers '(python-mypy))
              ;; (setq python-indent-offset 4) ; default 4
              ))

  (add-hook 'text-mode-hook my-mode-setup)

  ;; from package
  (with-eval-after-load 'markdown-mode
    (require 'my-package))

  (with-eval-after-load 'plantuml-mode
    (require 'my-package))
  (add-hook 'plantuml-mode-hook
            (lambda ()
              (funcall my-prog-mode-setup 2)
              (flycheck-mode)
              (flycheck-plantuml-setup)
              (setq plantuml-indent-level 2)
              ))

  (with-eval-after-load 'web-mode
    (require 'my-package))
  (add-hook 'web-mode-hook
            (lambda ()
              (funcall my-prog-mode-setup 2)
              ;; (highlight-indent-guides-mode)

              (setq web-mode-markup-indent-offset 2)
              (setq web-mode-css-indent-offset 2)
              (setq web-mode-code-indent-offset 2)
              )))

;;------------------------------------------------------------------------------
;; auto-mode-alist
;;------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.elc\\'" . fundamental-mode))
((add-to-list 'auto-mode-alist '("\\.\\(html?\\|php\\|tpl\\|c?[jt]s\\(on\\|x\\)?\\|vue\\)\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.puml?\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;;; init.el ends here
