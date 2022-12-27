;;; -*- coding: utf-8; lexical-binding: t -*-

;;==============================================================================
;; from package
;;==============================================================================
;;------------------------------------------------------------------------------
;; cygwin-mount
;; teach EMACS about cygwin styles and mount points
;; https://www.emacswiki.org/emacs/cygwin-mount.el
;;------------------------------------------------------------------------------
(eval-when-compile
  (when (eq system-type 'windows-nt)
    (require 'cygwin-mount)
    (cygwin-mount-build-table-internal)
    (cygwin-mount-activate))

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
;; cygwin's pty feature for NTEmacs
;; https://github.com/d5884/fakecygpty
;;------------------------------------------------------------------------------
(eval-when-compile
  (when (eq system-type 'windows-nt)
    (require 'fakecygpty))

  (defmacro fakecygpty-nt ()
    (when (eq system-type 'windows-nt)
      (let ((program-path-regexps (concat "^" (expand-file-name "/") "\\(usr/\\(local/\\)?\\)?bin/.*")))
        `(progn
           (autoload 'fakecygpty-activate "fakecygpty" t nil)
           (add-hook 'after-init-hook #'fakecygpty-activate)

           (with-eval-after-load 'fakecygpty
             (fset 'ad-fakecygpty--ignored-program
                   (lambda (program)
                     (not (string-match-p ,program-path-regexps (executable-find (file-name-nondirectory program))))))
             (advice-add 'fakecygpty--ignored-program :before-until 'ad-fakecygpty--ignored-program))
           )))))
(fakecygpty-nt)

;;------------------------------------------------------------------------------
;; shebang-exec
;; https://qiita.com/s-fubuki/items/67ddcf36970cd5492d2c
;; https://gist.github.com/s-fubuki/819a7f7e7cb2cbcaa5b9bd04dcb1e713
;;------------------------------------------------------------------------------
;; (eval-when-compile
;;   (defmacro shebang-exec-nt ()
;;     (when (eq system-type 'windows-nt)
;;       `(progn
;;          (add-hook 'after-init-hook (lambda () (require 'shebang-exec)))

;;          (with-eval-after-load 'shebang-exec
;;            (fset 'ad-executable-find
;;                  (lambda (&rest args)
;;                    (locate-file (car args) exec-path nil 'file-executable-p)
;;                    ))
;;            (advice-add 'executable-find :after-until 'ad-executable-find))
;;          ))))
;; (shebang-exec-nt)

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
;; a modular completion framework
;;------------------------------------------------------------------------------
;; company-box
;; a company front-end with icons
;;------------------------------------------------------------------------------
(eval-when-compile (require 'company))

(with-eval-after-load 'company
  (setq company-idle-delay nil)
  (setq company-minimum-prefix-length 3) ; minimum prefix length for idle completion
  (setq company-selection-wrap-around t)
  (define-key (default-value 'company-mode-map) (kbd "C-<tab>") 'company-complete)
  (add-hook 'company-mode-hook 'company-box-mode))

(eval-when-compile
  (require 'company-box)
  (require 'frame-local)
  (declare-function company-box--get-frame "company-box")
  (declare-function frame-local-get "frame-local")

  (defmacro fix-company-box (get-frame-func &rest margs)
    `(lambda (&rest args)
       (let ((frame (,get-frame-func ,(car margs))))
         (when frame
           (set-frame-parameter frame 'tab-bar-lines 0)))
       args)))

(with-eval-after-load 'company-box
  (set-face-attribute 'company-tooltip-selection nil :foreground "wheat" :background "steelblue")
  (set-face-attribute 'company-tooltip nil :background "midnight blue")

  (fset 'ad-company-box--display (fix-company-box company-box--get-frame))
  (advice-add 'company-box--display :after 'ad-company-box--display)

  (fset 'ad-company-box-doc--show (fix-company-box frame-local-getq company-box-doc-frame))
  (advice-add 'company-box-doc--show :after 'ad-company-box-doc--show))

;;------------------------------------------------------------------------------
;; flycheck
;; on-the-fly syntax checking
;;------------------------------------------------------------------------------
(eval-when-compile (require 'flycheck))

(with-eval-after-load 'flycheck
  (setq flycheck-display-errors-delay 0.3)
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ; remove useless warnings
  (setq flycheck-emacs-lisp-load-path 'inherit) ; use the `load-path' of the current Emacs session
  (add-hook 'flycheck-mode-hook (lambda () (setq left-fringe-width 8)))) ; Enable left-fringe only in flycheck-mode

;;------------------------------------------------------------------------------
;; helm
;; an Emacs incremental and narrowing framework
;;------------------------------------------------------------------------------
(eval-when-compile (require 'helm-files))

(with-eval-after-load 'helm
  (setq helm-ff-auto-update-initial-value nil)) ; Disable autocomplete

;; key-bind
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-r") #'helm-recentf)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "C-c i") #'helm-imenu)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)

;;------------------------------------------------------------------------------
;; highlight-indent-guides
;; a minor mode highlights indentation levels via font-lock
;;------------------------------------------------------------------------------
(eval-when-compile (require 'highlight-indent-guides))

(with-eval-after-load 'highlight-indent-guides
  ;; (setq-default highlight-indent-guides-method 'character)
  ;; (setq-default highlight-indent-guides-auto-character-face-perc 100)
  ;; (setq-default highlight-indent-guides-character ?\|)
  ;; (setq-default highlight-indent-guides-method 'column)
  (setq highlight-indent-guides-auto-odd-face-perc 40)
  (setq highlight-indent-guides-auto-even-face-perc 25))

;;------------------------------------------------------------------------------
;; irony
;; a C/C++ minor mode powered by libclang
;;------------------------------------------------------------------------------
(eval-when-compile
  (require 'irony)

  (defmacro irony-nt ()
    (when (eq system-type 'windows-nt)
      `(progn
         (with-eval-after-load 'irony
           ;; Set the buffer size to 64K on Windows (from the original 4K)
           (setq irony-server-w32-pipe-buffer-size (* 64 1024))

           ;; fixed issue with irony-server-install
           (fset 'ad-irony--install-server-read-command
                 (lambda (args)
                   "modify irony--install-server-read-command"
                   (setenv "CC" "clang") (setenv "CXX" "clang++") ; set clang to compiler
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
;; a Git porcelain inside Emacs
;;------------------------------------------------------------------------------
;; (eval-when-compile
;;   (defmacro magit-nt ()
;;     (when (and (eq system-type 'windows-nt) (fboundp 'start-my-shell-process))
;;       `(progn
;;          (with-eval-after-load 'magit-section
;;            (start-my-shell-process))
;;          ))))
;; (magit-nt)

(with-eval-after-load 'magit
  (eval-when-compile (defvar git-commit-summary-max-length))

  (setq git-commit-summary-max-length 999)
  (add-hook 'git-commit-setup-hook 'turn-off-auto-fill t))

;;------------------------------------------------------------------------------
;; markdown-mode
;;------------------------------------------------------------------------------
(with-eval-after-load 'markdown-mode
  (eval-when-compile (require 'markdown-mode))
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-content-type "application/xhtml+xml")
  (setq markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"))
  (setq markdown-xhtml-header-content "
<style>
body {
  box-sizing: border-box;
  width: 100%;
  margin: 40px auto;
  padding: 0 10px;
}
</style>
<script>
document.addEventListener('DOMContentLoaded', () => {
  document.body.classList.add('markdown-body');
});
</script>
" ))

;;------------------------------------------------------------------------------
;; migemo
;; Japanese incremental search through dynamic pattern expansion
;;------------------------------------------------------------------------------
(with-eval-after-load 'migemo
  (fset 'ad-migemo-register-isearch-keybinding
        (lambda ()
          (define-key isearch-mode-map (kbd "C-M-y") 'migemo-isearch-yank-char)
          (define-key isearch-mode-map (kbd "C-w") 'migemo-isearch-yank-word)
          (define-key isearch-mode-map (kbd "M-s C-e") 'migemo-isearch-yank-line)
          (define-key isearch-mode-map (kbd "M-m") 'migemo-isearch-toggle-migemo)
          (define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)))
  (advice-add 'migemo-register-isearch-keybinding :override 'ad-migemo-register-isearch-keybinding))
(add-hook 'isearch-mode-hook (lambda () (require 'cmigemo)))

;; ;; ------------------------------------------------------------------------------
;; ;; plantuml-mode
;; ;; ------------------------------------------------------------------------------
;; (with-eval-after-load 'plantuml-mode
;;   (eval-when-compile (require 'plantuml-mode))

;;   (setq plantuml-jar-path
;;         (eval-when-compile (locate-file "plantuml" exec-path '(".jar"))))

;;   ;; specify the desired output type to use for generated diagrams(svg,png,txt)
;;   (setq-default plantuml-output-type "png")

;;   (setq plantuml-default-exec-mode 'jar)

;;   (eval-and-compile (require 'smart-compile))
;;   (let ((sc-cmd (concat "java -jar " plantuml-jar-path " -charset UTF-8 -tsvg %f")))
;;     (add-to-list 'smart-compile-alist `("\\.puml?\\'" . ,sc-cmd) t)
;;     ))

;;------------------------------------------------------------------------------
;; smert compile
;;------------------------------------------------------------------------------
(eval-when-compile (require 'smart-compile))

(with-eval-after-load 'smart-compile
  (let ((cmd (lambda ()
               (emacs-lisp-byte-compile)
               (native-compile buffer-file-name))))
    (add-to-list 'smart-compile-alist `(emacs-lisp-mode ,cmd))))
(global-set-key (kbd "C-c c") #'smart-compile)

;;------------------------------------------------------------------------------
;; web-mode
;;------------------------------------------------------------------------------
(with-eval-after-load 'web-mode
  (eval-when-compile (require 'web-mode))

  (setq web-mode-engines-alist '(("php" . "\\.phtml\\'")))

  (setq web-mode-content-types-alist '(("js" . "\\.\\(js[x]\\|vue\\)?\\'")))
  (add-to-list 'web-mode-comment-formats '("javascript" . "//" ))

  (setq indent-tabs-mode nil)
  (setq web-mode-enable-current-element-highlight t))
