;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; auto-insert
;; ファイルの種類に応じたテンプレートの挿入
;; emacs default
;;------------------------------------------------------------------------------

(add-hook 'find-file-not-found-hooks 'auto-insert)

(with-eval-after-load 'autoinsert
  ;; テンプレートのディレクトリ
  (defvar auto-insert-directory)
  (setq auto-insert-directory "~/.emacs.d/auto-insert/")

  ;; テンプレート中で展開してほしいテンプレート変数を定義
  (defvar template-replacements-alists)
  (setq template-replacements-alists
        `(("%file%"             . ,(lambda () (file-name-nondirectory (buffer-file-name))))
          ("%file-without-ext%" . ,(lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
          ("%include-guard%"    . ,(lambda () (format "INCLUDE_%s_H" (upcase (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))))))

  (defun my-template ()
    (time-stamp)
    (dolist (c template-replacements-alists)
      (goto-char (point-min))
      (while (search-forward (car c) nil t)
        (replace-match (funcall (cdr c)))))
    (goto-char (point-max))
    (message "done."))

  ;; 各ファイルによってテンプレートを切り替える
  (defvar auto-insert-alist)
  (setq auto-insert-alist
        (append '(("\\.cpp$"   . ["template.cpp" my-template])
                  ("\\.h$"     . ["template.h" my-template])
                  ("Makefile$" . ["template.make" my-template]))
                auto-insert-alist)))
