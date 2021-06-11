;;; -*- coding: utf-8; lexical-binding: t -*-

;;==============================================================================
;; from package
;;==============================================================================
;;------------------------------------------------------------------------------
;; company
;; 補完システム
;;------------------------------------------------------------------------------
(with-eval-after-load 'company
  (setq-default company-idle-delay nil) ;; 遅延
  (setq-default company-minimum-prefix-length 3) ;; 補完開始文字長
  (setq-default company-selection-wrap-around t) ;; 最下時に↓で最初に戻る
  (define-key (default-value 'company-mode-map) (kbd "C-<tab>") 'company-complete))

;;------------------------------------------------------------------------------
;; company-box
;; A company front-end with icons.
;;------------------------------------------------------------------------------
(with-eval-after-load 'company
  (add-hook 'company-mode-hook 'company-box-mode)
  (setq-default company-box-enable-icon nil)
  ;;(setq-default company-box-icons-alist 'company-box-icons-all-the-icons)
  ;;(setq-default company-box-doc-enable nil)
  (set-face-attribute 'company-tooltip-selection nil :foreground "wheat" :background "steelblue")
  (set-face-attribute 'company-tooltip nil :background "midnight blue")
  )

;;------------------------------------------------------------------------------
;; elscreen
;; ウィンドウ構成管理
;;------------------------------------------------------------------------------
;; (with-eval-after-load 'elscreen
;;   (setq-default elscreen-display-tab t) ;; tabの表示および幅の設定
;;   (setq-default elscreen-display-screen-number nil) ;; modelineへの番号表示
;;   (setq-default elscreen-tab-display-kill-screen nil) ;; タブの先頭に[X]を表示しない
;;   (setq-default elscreen-tab-display-control nil) ;; header-lineの先頭に[<->]を表示しない

;;   ;; face
;;   ;; tab-background (header-line)
;;   (set-face-attribute 'elscreen-tab-background-face nil :background "black" :underline nil)
;;   ;; tab-current-screen (current-screen)
;;   (set-face-attribute 'elscreen-tab-current-screen-face nil :foreground "yellow" :background "black" :underline nil)
;;   ;; tab-other-screen (other-screen)
;;   (set-face-attribute 'elscreen-tab-other-screen-face nil :foreground "Gray72" :background "black" :underline nil)
;;   )

;;------------------------------------------------------------------------------
;; exec-path-from-shell
;; シェルと環境変数を同期
;;------------------------------------------------------------------------------
(eval-when-compile
  (require 'exec-path-from-shell)
  (let ((add-env-vars '()))
    (when (eq system-type 'windows-nt)
      (setq add-env-vars (append add-env-vars '("LANG" "PKG_CONFIG_PATH" "http_proxy" "https_proxy"))))
    (mapc (lambda (x) (add-to-list 'exec-path-from-shell-variables x t)) add-env-vars))

  (defmacro setenv_cached-env-var (env-var-lst)
    (cons 'progn (mapcar (lambda (x) `(setenv ,x ,(getenv x))) (eval env-var-lst))))

  (defmacro copy-envs-settings ()
    (when (string-match ".el$" (or (locate-library "-packages-") ""))
      ;; sync emacs environment variable with shell's one
      (exec-path-from-shell-initialize))

    `(cond ((time-less-p ',(nth 5 (file-attributes "~/.bash_profile")) (nth 5 (file-attributes "~/.bash_profile")))
            ;; sync emacs environment variable with shell's one
            (exec-path-from-shell-copy-envs ',exec-path-from-shell-variables)
            (add-hook 'after-init-hook (lambda () (byte-compile-file (locate-library "-packages-.el")))))
           (t ;; else
            ;; setenv cached environment variables
            (setenv_cached-env-var exec-path-from-shell-variables)
            (setq exec-path (append (split-string (getenv "PATH") path-separator) ',(last exec-path)))))))

(when (string= "0" (getenv "SHLVL"))
  (copy-envs-settings))

;;------------------------------------------------------------------------------
;; elscreen-separate-buffer-list
;; screenごとに独自のバッファリスト
;;------------------------------------------------------------------------------
;; (autoload 'elscreen-separate-buffer-list-mode "elscreen-separate-buffer-list" t)
;; (elscreen-separate-buffer-list-mode 1)

;;------------------------------------------------------------------------------
;; flycheck
;; エラーチェッカー
;;------------------------------------------------------------------------------
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook (lambda () (setq left-fringe-width 8))) ;; 左フリンジを有効化

  (setq-default flycheck-display-errors-delay 0.3) ;; 遅延
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;; remove useless warnings
  (setq-default flycheck-emacs-lisp-load-path 'inherit) ;; use the `load-path' of the current Emacs session

  (declare-function flycheck-add-mode "flycheck")
  (flycheck-add-mode 'emacs-lisp 'elisp-mode))

;;------------------------------------------------------------------------------
;; helm
;; emacsに統一的なある”操作方式”を提供するフレームワーク
;;------------------------------------------------------------------------------
(with-eval-after-load 'helm
  ;; helm-migemo を有効化
  ;; (or (featurep 'migemo) (require 'cmigemo))
  ;; (declare-function helm-migemo-mode "helm-multi-match")
  ;; (helm-migemo-mode)

  ;; http://fukuyama.co/nonexpansion
  ;; 自動補完を無効にする
  (setq-default helm-ff-auto-update-initial-value nil))

;; key-bind
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-r") #'helm-recentf)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "C-c i") #'helm-imenu)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)

;;------------------------------------------------------------------------------
;; highlight-indent-guides
;; A minor mode highlights indentation levels via font-lock.
;;------------------------------------------------------------------------------
(with-eval-after-load 'highlight-indent-guides
  ;; (setq-default highlight-indent-guides-method 'character)
  ;; (setq-default highlight-indent-guides-auto-character-face-perc 100)
  ;; (setq-default highlight-indent-guides-character ?\|)
  ;; (setq-default highlight-indent-guides-method 'column)
  (setq-default highlight-indent-guides-auto-odd-face-perc 40)
  (setq-default highlight-indent-guides-auto-even-face-perc 25))

;;------------------------------------------------------------------------------
;; irony-eldoc
;; irony-mode support for eldoc-mode
;; from https://github.com/josteink/irony-eldoc
;; 本家の更新がないのでフォーク版を使用
;;------------------------------------------------------------------------------
;; (autoload 'irony-eldoc "irony-eldoc")
;; (add-hook 'irony-mode-hook 'irony-eldoc)

;; ;; 文字化け対処
;; (defun ad-irony-eldoc--strip-underscores (string)
;;   (if (or (not string) (not (default-value 'irony-eldoc-strip-underscores)))
;;       string
;;     (let ((new-string string)
;;           (regexps '(("\\_<_+" . ""))))
;;       (dolist (r regexps)
;;         (setq new-string
;;               (replace-regexp-in-string (car r) (cdr r) new-string)))
;;       new-string)))
;; (advice-add 'irony-eldoc--strip-underscores :override 'ad-irony-eldoc--strip-underscores)

;;------------------------------------------------------------------------------
;; magit
;; git クライアント
;;------------------------------------------------------------------------------
(with-eval-after-load 'magit
  (setq-default git-commit-summary-max-length 999))

;;------------------------------------------------------------------------------
;; migemo
;; ローマ字入力で日本語文字列を検索
;;------------------------------------------------------------------------------
(fset 'ad-migemo-register-isearch-keybinding
      (lambda ()
        (define-key isearch-mode-map (kbd "C-M-y") 'migemo-isearch-yank-char)
        (define-key isearch-mode-map (kbd "C-w") 'migemo-isearch-yank-word)
        (define-key isearch-mode-map (kbd "M-s C-e") 'migemo-isearch-yank-line)
        (define-key isearch-mode-map (kbd "M-m") 'migemo-isearch-toggle-migemo)
        (define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)))
(advice-add 'migemo-register-isearch-keybinding :override 'ad-migemo-register-isearch-keybinding)
(add-hook 'isearch-mode-hook (lambda () (require 'cmigemo)))

;; ------------------------------------------------------------------------------
;; plantuml-mode
;; ------------------------------------------------------------------------------
;; (eval-when-compile (require 'plantuml-mode)
;;                    (require 'smart-compile))
;; (with-eval-after-load 'plantuml-mode
;;   (setq-default plantuml-jar-path "c:/msys64/usr/local/bin/plantuml.jar")

;;   ;; javaにオプションを渡したい場合はここにかく
;;   (setq-default plantuml-java-options "")

;;   ;; plantumlのプレビュー出力形式(svg,png,txt,utxt)
;;   ;; (setq-default plantuml-output-type "txt")

;;   (setq-default plantuml-default-exec-mode 'jar)

;;   (with-eval-after-load 'smart-compile
;;     (let ((sc-cmd (concat "java -jar " plantuml-jar-path " -charset UTF-8 -tsvg %f")))
;;       (add-to-list 'smart-compile-alist `("\\.pum$" . ,sc-cmd) t)
;;       )))

;;------------------------------------------------------------------------------
;; shell-pop
;; シェルバッファをポップアップ
;;------------------------------------------------------------------------------
;; (global-set-key [f8] 'shell-pop)

;;------------------------------------------------------------------------------
;; smert compile
;;------------------------------------------------------------------------------
(eval-when-compile (require 'smart-compile))
(with-eval-after-load 'smart-compile
  (add-to-list 'smart-compile-alist '(elisp-mode emacs-lisp-byte-compile)))
(global-set-key (kbd "C-c c") #'smart-compile)

;;------------------------------------------------------------------------------
;; vs-set-c-style
;; Visual Studio スタイルのインデント設定
;; http://yohshiy.blog.fc2.com/blog-entry-264.html
;;------------------------------------------------------------------------------
;; (autoload 'vs-set-c-style "vs-set-c-style")
