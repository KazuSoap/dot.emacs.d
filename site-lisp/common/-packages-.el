;;; -*- coding: utf-8; lexical-binding: t -*-

;;==============================================================================
;; from package
;;==============================================================================
;;------------------------------------------------------------------------------
;; cygwin-mount
;; Teach EMACS about cygwin styles and mount points.
;; https://www.emacswiki.org/emacs/cygwin-mount.el
;;------------------------------------------------------------------------------
(eval-when-compile
  (when (eq system-type 'windows-nt)
    (require 'cygwin-mount)
    (defvar msys-bin (file-name-directory shell-file-name))
    (defvar cygwin-mount-table--internal)

    (setq-default cygwin-mount-program (concat msys-bin "mount.exe"))
    (setq-default cygwin-mount-uname-program (concat msys-bin "uname.exe"))
    (cygwin-mount-build-table-internal))

  (defmacro cygwin-mount-nt ()
    (when (eq system-type 'windows-nt)
      `(progn
         (autoload 'cygwin-mount-activate "cygwin-mount" t nil)
         (add-hook 'after-init-hook #'cygwin-mount-activate)

         (with-eval-after-load 'cygwin-mount
           (setq cygwin-mount-table ',cygwin-mount-table--internal)
           (fset 'cygwin-mount-get-cygdrive-prefix (lambda () ,(cygwin-mount-get-cygdrive-prefix))))
         ))))
(cygwin-mount-nt)

;;------------------------------------------------------------------------------
;; fakecygpty
;; NTEmacs の仮想端末偽装
;; https://github.com/d5884/fakecygpty
;;------------------------------------------------------------------------------
(eval-when-compile
  (defmacro fakecygpty-nt ()
    (when (eq system-type 'windows-nt)
      `(progn
         (autoload 'fakecygpty-activate "fakecygpty" t nil)
         (add-hook 'after-init-hook #'fakecygpty-activate)
         ))))
(fakecygpty-nt)

;;------------------------------------------------------------------------------
;; exec-path-from-shell
;; sync emacs environment variable with shell's one
;;------------------------------------------------------------------------------
(eval-when-compile
  (require 'exec-path-from-shell)

  (defmacro setenv_cached-env-var (env-var-lst)
    (mapcar (lambda (x) `(setenv ,x ,(getenv x))) (eval env-var-lst)))

  (defmacro copy-envs-settings ()
    (when (and (eq system-type 'windows-nt) (fboundp 'cygpath))
      ;; convert path format from unix style to win-nt style
      (fset 'ad-exec-path-from-shell-setenv
            (lambda (args)
              (and (string= (car args) "PATH")
                   (setf (nth 1 args) (cygpath "-amp" (nth 1 args))))
              args))
      (advice-add 'exec-path-from-shell-setenv :filter-args 'ad-exec-path-from-shell-setenv)

      (let ((add-env-vars '()))
        (setq add-env-vars (append add-env-vars '("LANG" "PKG_CONFIG_PATH" "http_proxy" "https_proxy")))
        (mapc (lambda (x) (add-to-list 'exec-path-from-shell-variables x t)) add-env-vars))

      (exec-path-from-shell-initialize)

      `(progn
         (when (string= "0" (getenv "SHLVL"))
           ,@(macroexpand '(setenv_cached-env-var exec-path-from-shell-variables))
           (setq exec-path (append (split-string (getenv "PATH") path-separator) ',(last exec-path))))
         ))))
(copy-envs-settings)

;;------------------------------------------------------------------------------
;; IME
;;------------------------------------------------------------------------------
(eval-when-compile
  (defvar ime-settings-core nil)

  (cond ((eq system-type 'windows-nt) ; if
         (defvar w32-ime-mode-line-state-indicator)
         (defvar w32-ime-mode-line-state-indicator-list)
         (declare-function tr-ime-hook-check "tr-ime-hook")
         (declare-function w32-ime-wrap-function-to-control-ime "w32-ime")
         (declare-function tr-ime-font-reflect-frame-parameter "tr-ime-font")

         (setq ime-settings-core
               `(progn
                  ;; 無変換キーで tr-ime & w32-ime を有効化
                  (global-set-key (kbd "<non-convert>")
                                  (lambda ()
                                    (interactive)
                                    (global-unset-key (kbd "<non-convert>"))

                                    ;; tr-imeの有効化
                                    ;; (tr-ime-advanced-install)
                                    (tr-ime-advanced-initialize)
                                    (tr-ime-hook-check)))

                  (with-eval-after-load 'w32-ime
                    ;; 標準IMEの設定
                    (setq default-input-method "W32-IME")

                    ;; Windows IME の ON:[あ]/OFF:[Aa] をモードラインに表示
                    (setq w32-ime-mode-line-state-indicator "[Aa]")
                    (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

                    ;; IME の初期化
                    (w32-ime-initialize)

                    ;; IME ON/OFF時のカーソルカラー
                    (add-hook 'w32-ime-on-hook (lambda () (set-cursor-color "yellow")))
                    (add-hook 'w32-ime-off-hook (lambda () (set-cursor-color "thistle")))

                    ;; IMEの制御(yes/noをタイプするところでは IME をオフにする)
                    (w32-ime-wrap-function-to-control-ime #'universal-argument)
                    (w32-ime-wrap-function-to-control-ime #'read-string)
                    (w32-ime-wrap-function-to-control-ime #'read-char)
                    (w32-ime-wrap-function-to-control-ime #'read-from-minibuffer)
                    (w32-ime-wrap-function-to-control-ime #'y-or-n-p)
                    (w32-ime-wrap-function-to-control-ime #'yes-or-no-p)
                    (w32-ime-wrap-function-to-control-ime #'map-y-or-n-p)

                    ;; frame font
                    (modify-all-frames-parameters `((ime-font . ,(frame-parameter nil 'font))))
                    (tr-ime-font-reflect-frame-parameter))
                  )))

        ((eq system-type 'gnu/linux) ; else if
         (require 'mozc)

         (setq ime-settings-core
               `(progn
                  (declare-function mozc-clean-up-changes-on-buffer "mozc")
                  (declare-function mozc-fall-back-on-default-binding "mozc")

                  (setq default-input-method "japanese-mozc")

                  (with-eval-after-load 'mozc
                    ;; (setq mozc-candidate-style 'overlay)
                    ;; (require 'mozc-popup)
                    ;; (setq mozc-candidate-style 'popup)
                    (require 'mozc-cand-posframe)
                    (setq mozc-candidate-style 'posframe)

                    ;; 無変換キーのキーイベントを横取りする
                    (fset 'ad-mozc-intercept-keys
                          (lambda (f event)
                            (cond
                             ((member event '(muhenkan))
                              ;; (message "%s" event) ;debug
                              (mozc-clean-up-changes-on-buffer)
                              (mozc-fall-back-on-default-binding event))
                             (t ; else
                              ;; (message "%s" event) ;debug
                              (funcall f event)))))
                    (advice-add 'mozc-handle-event :around 'ad-mozc-intercept-keys))

                  (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
                  (global-set-key (kbd "<henkan>") (lambda () (interactive) (activate-input-method default-input-method)))
                  (global-set-key (kbd "<hiragana-katakana>") (lambda () (interactive) (activate-input-method default-input-method)))
                  (global-set-key (kbd "<muhenkan>") (lambda () (interactive) (deactivate-input-method)))

                  ;; IME ON/OFF時のカーソルカラー
                  (add-hook 'input-method-activate-hook (lambda () (set-cursor-color "yellow")))
                  (add-hook 'input-method-deactivate-hook (lambda () (set-cursor-color "thistle")))
                  ))))

  (defmacro ime-settings ()
    ime-settings-core
    ))
(ime-settings)

;;------------------------------------------------------------------------------
;; company
;; 補完システム
;;------------------------------------------------------------------------------
;; company-box
;; A company front-end with icons.
;;------------------------------------------------------------------------------
(with-eval-after-load 'company
  (eval-when-compile (require 'company))

  (setq company-idle-delay nil) ;; 遅延
  (setq company-minimum-prefix-length 3) ;; 補完開始文字長
  (setq company-selection-wrap-around t) ;; 最下時に↓で最初に戻る
  (define-key (default-value 'company-mode-map) (kbd "C-<tab>") 'company-complete)
  (add-hook 'company-mode-hook 'company-box-mode)
  )

(with-eval-after-load 'company-box
  (eval-when-compile (require 'company-box)
                     (require 'frame-local))

  (set-face-attribute 'company-tooltip-selection nil :foreground "wheat" :background "steelblue")
  (set-face-attribute 'company-tooltip nil :background "midnight blue")

  (fset 'ad-company-box--display
        (lambda (&rest args)
          (set-frame-parameter (company-box--get-frame) 'tab-bar-lines 0)
          args))
  (advice-add 'company-box--display :after 'ad-company-box--display)

  (fset 'ad-company-box-doc--show
        (lambda (&rest args)
          (set-frame-parameter (frame-local-getq company-box-doc-frame) 'tab-bar-lines 0)
          args))
  (advice-add 'company-box-doc--show :after 'ad-company-box-doc--show))

;;------------------------------------------------------------------------------
;; flycheck
;; エラーチェッカー
;;------------------------------------------------------------------------------
(with-eval-after-load 'flycheck
  (eval-when-compile (require 'flycheck))
  ;; (declare-function flycheck-add-mode "flycheck")

  (setq flycheck-display-errors-delay 0.3) ;; 遅延
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;; remove useless warnings
  (setq flycheck-emacs-lisp-load-path 'inherit) ;; use the `load-path' of the current Emacs session
  (add-hook 'flycheck-mode-hook (lambda () (setq left-fringe-width 8))) ;; 左フリンジを有効化
  ;; (flycheck-add-mode 'emacs-lisp 'elisp-mode)
  )

;;------------------------------------------------------------------------------
;; helm
;; emacsに統一的なある”操作方式”を提供するフレームワーク
;;------------------------------------------------------------------------------
(with-eval-after-load 'helm
  ;; helm autoload bug fix
  ;; -> Error running timer: (void-function helm-completion--flex-transform-pattern)
  (require 'helm-mode)

  (eval-when-compile (require 'helm-files))

  ;; 自動補完を無効にする
  (setq helm-ff-auto-update-initial-value nil)
  )

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
  (eval-when-compile (require 'highlight-indent-guides))

  ;; (setq-default highlight-indent-guides-method 'character)
  ;; (setq-default highlight-indent-guides-auto-character-face-perc 100)
  ;; (setq-default highlight-indent-guides-character ?\|)
  ;; (setq-default highlight-indent-guides-method 'column)
  (setq highlight-indent-guides-auto-odd-face-perc 40)
  (setq highlight-indent-guides-auto-even-face-perc 25)
  )

;;------------------------------------------------------------------------------
;; irony
;; A C/C++ minor mode powered by libclang
;;------------------------------------------------------------------------------
(eval-when-compile
  (require 'irony)

  (defmacro irony-nt ()
    (when (eq system-type 'windows-nt)
      `(progn
         (with-eval-after-load 'irony
           ;; Set the buffer size to 64K on Windows (from the original 4K)
           (setq irony-server-w32-pipe-buffer-size (* 64 1024))

           ;; irony-server-install に失敗する問題の修正
           (fset 'ad-irony--install-server-read-command
                 (lambda (args)
                   "modify irony--install-server-read-command"
                   (setenv "CC" "clang") (setenv "CXX" "clang++") ; コンパイラに clang を指定
                   `(,(replace-regexp-in-string
                       (format "^\\(%s\\)" (shell-quote-argument (default-value 'irony-cmake-executable)))
                       "\\1 -G'MSYS Makefiles'"
                       (car args)))))
           (advice-add 'irony--install-server-read-command :filter-args 'ad-irony--install-server-read-command)
           (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))
         ))))
(irony-nt)

;;------------------------------------------------------------------------------
;; lsp-mode
;; Language Server Protocol Support for Emacs
;;------------------------------------------------------------------------------
(with-eval-after-load 'lsp-mode
  (eval-when-compile
    (require 'lsp-mode)
    (require 'lsp-diagnostics)
    (require 'lsp-headerline))

  ;; .venv, .mypy_cache を watch 対象から外す
  (dolist (dir '("[/\\\\]\\.venv$"
                 "[/\\\\]\\.mypy_cache$"
                 "[/\\\\]__pycache__$"))
    (add-to-list 'lsp-file-watch-ignored dir))

  ;; lsp-mode の設定はここを参照してください。
  ;; https://emacs-lsp.github.io/lsp-mode/page/settings/

  (setq lsp-auto-configure t)
  (setq lsp-completion-enable t)

  ;; imenu-listを使う場合は(lsp-uiの imenu 統合を使わない場合は) nil にする
  ; ;(setq lsp-enable-imenu nil)

  ;; クロスリファレンスとの統合を有効化する
  ;; xref-find-definitions
  ;; xref-find-references
  (setq lsp-enable-xref t)

  ;; linter framework として flycheck を使う
  ;; (setq lsp-diagnostics-provider :flycheck)

  ;; ミニバッファでの関数情報表示
  (setq lsp-eldoc-enable-hover t)

  ;; nii: ミニバッファでの関数情報をシグニチャだけにする
  ;; t: ミニバッファでの関数情報で、doc-string 本体を表示する
  (setq lsp-eldoc-render-all nil)

  ;; breadcrumb
  ;; 最上部にパンくずリストを表示する。
  ;; https://emacs-lsp.github.io/lsp-mode/page/settings/headerline/#lsp-headerline-breadcrumb-segments
  ;; lsp-headerline-breadcrumb-segments に指定できるキーワードは以下の通り。
  ;;   project
  ;;   file
  ;;   path-up-to-project
  ;;   symbols
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(project file symbols))

  ;; snippet
  (setq lsp-enable-snippet t)
  )

;;------------------------------------------------------------------------------
;; lsp-ui
;; UI integrations for lsp-mode
;;------------------------------------------------------------------------------
(with-eval-after-load 'lsp-ui
  (eval-when-compile (require 'lsp-ui))

  ;; ui-peek を有効化する
  (setq lsp-ui-peek-enable t)

  ;; 候補が一つでも、常にpeek表示する。
  (setq lsp-ui-peek-always-show t)

  ;; sideline で flycheck 等の情報を表示する
  (setq lsp-ui-sideline-show-diagnostics t)

  ;; sideline で コードアクションを表示する
  (setq lsp-ui-sideline-show-code-actions t)

  ;; ホバーで表示されるものを、ホバーの変わりにsidelineで表示する
  ;;(setq lsp-ui-sideline-show-hover t)
  )

;;------------------------------------------------------------------------------
;; magit
;; git クライアント
;;------------------------------------------------------------------------------
;; (eval-when-compile
;;   (defmacro magit-nt ()
;;     (when (and (eq system-type 'windows-nt) (fboundp 'start-my-shell-process))
;;       `(progn
;;          (with-eval-after-load 'magit-utils
;;            (start-my-shell-process))
;;          ))))
;; (magit-nt)

(with-eval-after-load 'magit
  (eval-when-compile (defvar git-commit-summary-max-length))

  (setq git-commit-summary-max-length 999)
  (add-hook 'git-commit-setup-hook 'turn-off-auto-fill t)
  )

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
;; smert compile
;;------------------------------------------------------------------------------
(with-eval-after-load 'smart-compile
  (eval-when-compile (require 'smart-compile))

  (let ((cmd
         (lambda ()
           (emacs-lisp-byte-compile)
           (native-compile buffer-file-name))))
    (add-to-list 'smart-compile-alist `(emacs-lisp-mode ,cmd))))
(global-set-key (kbd "C-c c") #'smart-compile)
