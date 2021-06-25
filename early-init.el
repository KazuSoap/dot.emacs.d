;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; windows-misc
;;------------------------------------------------------------------------------
(eval-when-compile
  ;; Get the installation location of "msys2"
  (defconst msys-root
    (let* ((reg_hkcu_uninstall_key "\"HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\"")
           (reg_query_cmd (concat "reg query " reg_hkcu_uninstall_key " -v InstallLocation -s | findstr msys64")))
      (ignore-errors
        (expand-file-name
         (replace-regexp-in-string
          "+$" ""
          (nth 3 (split-string (shell-command-to-string reg_query_cmd) " +\\|\n")))))))

  ;; Windows Specific Settings
  (defmacro windows-nt-core ()
    (when (eq system-type 'windows-nt)
      `(progn
         ;; Set environment variable
         (setenv "HOME" (concat ,(concat msys-root "/home/") user-login-name))
         (setenv "SHELL" ,(concat msys-root "/usr/bin/bash"))
         (setenv "MSYSTEM" "MINGW64")
         (or (getenv "SHLVL") (setenv "SHLVL" "0"))
         (setq shell-file-name ,(getenv "SHELL"))

         ;; Set the default char-code in the following cases;
         ;; (1) when creating a new file,
         ;; (2) subprocess I/O,
         ;; (3) if not set elsewhere.
         (prefer-coding-system 'utf-8-unix)

         (fset 'cygpath
               (lambda (&optional option path)
                 "cygpath for emacs lisp"
                 (when path
                   (with-temp-buffer
                     (call-process ,(concat msys-root "/usr/bin/cygpath") nil '(t nil) nil option path)
                     (unless (bobp)
                       (goto-char (point-min))
                       (buffer-substring-no-properties (point) (line-end-position)))))))))))
(windows-nt-core)

;;------------------------------------------------------------------------------
;; common-misc
;;------------------------------------------------------------------------------
;; garbage collection
(setq gc-cons-threshold (* 128 1024 1024))

;; hide startup-message
(setq inhibit-startup-screen t)

;; show file path in title bar
(setq frame-title-format '((:eval (if (buffer-file-name) "%f" "%b")) " - Emacs"))

;; don't beep
(setq ring-bell-function #'ignore)

;; don't make backup file
(setq auto-save-default nil  ; #*
      make-backup-files nil) ; *.~

;; mouse scroll
(setq mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control))))

;; package
(setq custom-file (eval-when-compile (concat user-emacs-directory "my-custom-file.el")))

;; major mode of *scratch* buffer
;; (setq initial-major-mode #'fundamental-mode)

;; default major-mode
(setq-default major-mode #'text-mode)

;; indent with half-width spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; bidirectional text
(setq-default bidi-display-reordering nil) ; Set whether to allow bidirectional text.
;; (setq-default bidi-paragraph-direction 'left-to-right) ; Force text direction (default)

;; Set "yes" to "y" and "no" to "n"
(fset 'yes-or-no-p #'y-or-n-p)

;; translate "C-h" to "Back Space"
(define-key key-translation-map [?\C-h] [?\C-?])

;; show startup time in [ms]
(add-hook 'after-init-hook
          (lambda ()
            (let* ((sec (string-to-number (emacs-init-time)))
                   (ms (* 1000 sec)))
              (message "Emacs loaded in %.3f ms" ms))))

;;------------------------------------------------------------------------------
;; face & frame parameters
;;------------------------------------------------------------------------------
;; color-theme
(load-theme 'wheatgrass t)
(set-face-attribute 'mode-line nil :foreground "gray85" :background "#4a5459")
(set-face-attribute 'fringe nil :background "black")

;; fontset
;; XLFD
;; https://wiki.archlinux.jp/index.php/X_Logical_Font_Description
;; https://qiita.com/j8takagi/items/01aecdd28f87cdd3cd2c
;; -maker-family-weight-slant-widthtype-style-px-height-resX-resY-spacing-width-registry-encoding
(create-fontset-from-fontset-spec
 (eval-when-compile
   (let ((fontset-base
          (cond ((eq system-type 'windows-nt)
                 "-outline-ricty diminished discord-bold-normal-normal-mono-*-*-*-*-c-*-fontset-myricty")
                (t ; else
                 "-PfEd-ricty diminished discord-bold-normal-normal-*-*-*-*-*-m-0-fontset-myricty")))
         (ascii-font "ascii:-*-*-*-*-*-*-14-*-*-*-*-*-iso10646-1")
         (unicode-font "unicode:-*-*-*-*-*-*-*-*-*-*-*-*-iso10646-1"))
     (mapconcat #'identity `(,fontset-base ,ascii-font ,unicode-font) ","))))

;; frame parameters
(setq default-frame-alist
      '((width . 100)
        (height . 30)
        (alpha . 85)
        (font . "fontset-myricty")))

;;------------------------------------------------------------------------------
;; global minor-mode
;;------------------------------------------------------------------------------
(tool-bar-mode -1) ; default on
;; (menu-bar-mode -1) ; default on
(fringe-mode -1) ; default on
(column-number-mode 1) ; default off
;; (line-number-mode -1) ; default on
;; (size-indication-mode 1) ; default off
(delete-selection-mode 1) ; default off

;;------------------------------------------------------------------------------
;; load path
;;------------------------------------------------------------------------------
(let ((default-directory (eval-when-compile (concat user-emacs-directory "site-lisp"))))
  (normal-top-level-add-subdirs-to-load-path))

;;------------------------------------------------------------------------------
;; local functions
;;------------------------------------------------------------------------------
;;; my-revert-buffer-no-confirm ------------------------------------------------
;; https://www.emacswiki.org/emacs/RevertBuffer
(fset 'my-revert-buffer-no-confirm
      (lambda (&optional force-reverting)
        (interactive "P")
        (if (or force-reverting (not (buffer-modified-p)))
            (revert-buffer :ignore-auto :noconfirm)
          (error "The buffer has been modified"))))
(global-set-key (kbd "<f5>") 'my-revert-buffer-no-confirm)

;;; my-mark-eob ----------------------------------------------------------------
;; バッファの最後に "[EOB]" を表示
;; http://www.emacswiki.org/cgi-bin/wiki?HighlightEndOfBuffer
(fset 'my-mark-eob
      (lambda ()
        (let ((existing-overlays (overlays-in (point-max) (point-max)))
              (eob-mark (make-overlay (point-max) (point-max) nil t t))
              (eob-text "[EOB]"))
          ;; Delete any previous EOB markers.  Necessary so that they don't
          ;; accumulate on calls to revert-buffer.
          (dolist (next-overlay existing-overlays)
            (if (overlay-get next-overlay 'eob-overlay)
                (delete-overlay next-overlay)))
          ;; Add a new EOB marker.
          (put-text-property 0 (length eob-text)
                             'face '(foreground-color . "slate gray") eob-text)
          (overlay-put eob-mark 'eob-overlay t)
          (overlay-put eob-mark 'after-string eob-text))))
(add-hook 'find-file-hook 'my-mark-eob)

;;==============================================================================
;; emacs built in package
;;==============================================================================
;;------------------------------------------------------------------------------
;; auto-insert
;; ファイルの種類に応じたテンプレートの挿入
;;------------------------------------------------------------------------------
;; (eval-when-compile (require 'autoinsert))
;; (with-eval-after-load 'autoinsert
;;   ;; テンプレートのディレクトリ
;;   (setq-default auto-insert-directory (eval-when-compile (expand-file-name (concat user-emacs-directory "auto-insert"))))

;;   ;; テンプレート中で展開してほしいテンプレート変数を定義
;;   (setq-default template-replacements-alists
;;                 `(("%file%"             . ,(lambda () (file-name-nondirectory (buffer-file-name))))
;;                   ("%file-without-ext%" . ,(lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
;;                   ("%include-guard%"    . ,(lambda () (format "INCLUDE_%s_H" (upcase (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))))))

;;   (fset 'my-template
;;         (lambda ()
;;           (time-stamp)
;;           (dolist (c (default-value 'template-replacements-alists))
;;             (goto-char (point-min))
;;             (while (search-forward (car c) nil t)
;;               (replace-match (funcall (cdr c)))))
;;           (goto-char (point-max))
;;           (message "done.")))

;;   ;; 各ファイルによってテンプレートを切り替える
;;   (add-to-list 'auto-insert-alist '("\\.cpp$"   . ["template.cpp" my-template]))
;;   (add-to-list 'auto-insert-alist '("\\.h$"     . ["template.h" my-template]))
;;   (add-to-list 'auto-insert-alist '("Makefile$" . ["template.make" my-template])))

;; (add-hook 'find-file-not-found-functions #'auto-insert)

;;------------------------------------------------------------------------------
;; cua-mode
;; C-Ret で矩形選択
;;------------------------------------------------------------------------------
;; 詳しいキーバインド操作：http://dev.ariel-networks.com/articles/emacs/part5/
;; (with-eval-after-load 'cua-base
;;   (setq-default cua-enable-cua-keys nil))
(global-set-key (kbd "C-<return>") 'cua-rectangle-mark-mode)

;;------------------------------------------------------------------------------
;; display-line-numbers-mode
;; 行番号の表示
;;------------------------------------------------------------------------------
(with-eval-after-load 'display-line-numbers
  (set-face-attribute 'line-number nil :background "gray10")
  (set-face-attribute 'line-number-current-line nil :background "gray40"))

;;------------------------------------------------------------------------------
;; display-time
;; 時刻の表示
;;------------------------------------------------------------------------------
;; (setq-default display-time-string-forms
;;       '((format "%s/%s/%s(%s) %s:%s " year month day dayname 24-hours minutes)
;;         load))
;; (setq-default display-time-24hr-format t)
;; (display-time)

;;------------------------------------------------------------------------------
;; ediff
;;------------------------------------------------------------------------------
(with-eval-after-load 'ediff
  (eval-when-compile (require 'ediff))
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;------------------------------------------------------------------------------
;; eldoc
;;------------------------------------------------------------------------------
;; (with-eval-after-load 'eldoc
;;   (setq-default eldoc-idle-delay 0.5)) ;; eldoc 遅延

;;------------------------------------------------------------------------------
;; GDB
;; デバッガ
;;------------------------------------------------------------------------------
;; ;;; 有用なバッファを開くモード
;; (setq-default gdb-many-windows t)

;; ;;; 変数の上にマウスカーソルを置くと値を表示
;; (add-hook 'gdb-mode-hook 'gud-tooltip-mode)

;; ;;; I/O バッファを表示
;; (setq-default gdb-use-separate-io-buffer t)

;; ;;; t にすると mini buffer に値が表示される
;; (setq-default gud-tooltip-echo-area nil)

;;------------------------------------------------------------------------------
;; tab-bar-mode
;; frame-local tabs with named persistent window configurations
;;------------------------------------------------------------------------------
(fset 'my-tab-bar-mode-setup
      (lambda ()
        (tab-bar-history-mode +1)

        (setq tab-bar-show 1)
        (setq tab-bar-new-button-show nil)
        (setq tab-bar-close-button-show nil)
        (setq tab-bar-tab-hints t)
        (setq tab-bar-tab-name-function #'tab-bar-tab-name-current-with-count)
        (setq tab-bar-back-button "<")
        (setq tab-bar-forward-button ">")

        ;; face
        ;; tab-bar (header-line)
        (set-face-attribute 'tab-bar nil :font "fontset-myricty" :foreground "Gray72" :background "black")
        ;; tab-bar-tab (selected)
        (set-face-attribute 'tab-bar-tab nil :foreground "yellow" :background "black" :box nil)
        ;; tab-bar-tab-inactive (non-selected)
        (set-face-attribute 'tab-bar-tab-inactive nil :foreground "Gray72" :background "black")
        ))
(add-hook 'tab-bar-mode-hook 'my-tab-bar-mode-setup)

;;------------------------------------------------------------------------------
;; TRAMP(TransparentRemoteAccessMultipleProtocol)
;; edit remoto file from local emacs
;;------------------------------------------------------------------------------
(with-eval-after-load 'tramp
  (eval-when-compile (require 'tramp))
  (declare-function tramp-change-syntax "tramp")
  (tramp-change-syntax 'simplified) ;; Emacs 26.1 or later
  (setq tramp-encoding-shell "bash")

  ;; リモートサーバで shell を開いた時に日本語が文字化けしないよう、LC_ALL の設定を無効にする
  ;; http://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-processes.html#Running%20a%20debugger%20on%20a%20remote%20host
  (let ((process-environment (default-value 'tramp-remote-process-environment)))
    (setenv "LC_ALL" nil)
    (setq tramp-remote-process-environment process-environment)))

;;------------------------------------------------------------------------------
;; uniquify
;; 同一ファイル名を区別する
;;------------------------------------------------------------------------------
;; 表示形式指定(default: 'post-forward-angle-brackets)
;; (setq-default uniquify-buffer-name-style 'post-forward-angle-brackets)

;; 無視するバッファ名
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;------------------------------------------------------------------------------
;; whitespace-mode
;; 不可視文字の可視化
;;------------------------------------------------------------------------------
(with-eval-after-load 'whitespace
  (eval-when-compile (require 'whitespace))
  ;; 保存時に行末の空白を削除する
  (add-hook 'before-save-hook #'delete-trailing-whitespace)

  ;; 可視化する不可視文字のリスト
  (setq whitespace-style '(face trailing tabs spaces newline space-mark tab-mark newline-mark))

  ;; 表示の変更
  (setq whitespace-display-mappings
        '(;; space > " "
          (space-mark   ?\xA0   [?\u00A4]     [?_])
          (space-mark   ?\x8A0  [?\x8A4]      [?_])
          (space-mark   ?\x920  [?\x924]      [?_])
          (space-mark   ?\xE20  [?\xE24]      [?_])
          (space-mark   ?\xF20  [?\xF24]      [?_])
          ;; full-width-space > "□"
          (space-mark   ?\u3000 [?\u25a1]     [?_ ?_])
          ;; tab > "»" with underline
          (tab-mark     ?\t     [?\xBB ?\t]   [?\\ ?\t])
          ;; newline > "｣"
          (newline-mark ?\n     [?\uFF63 ?\n] [?$ ?\n])))

  ;; 以下の正規表現にマッチするものを"space"と認識
  (setq whitespace-space-regexp "\\(\u3000+\\)")

  ;; face
  (set-face-attribute 'whitespace-space nil :foreground "GreenYellow" :background "black")
  (set-face-attribute 'whitespace-tab nil :foreground "LightSkyBlue" :background "black" :underline t)
  (set-face-attribute 'whitespace-newline nil :foreground "DeepSkyBlue")
  (set-face-attribute 'whitespace-trailing nil :background "DeepPink"))

;;------------------------------------------------------------------------------
;; windmove
;; Emacsの分割ウィンドウを modifier-key + 矢印キー で移動
;;------------------------------------------------------------------------------
(eval-when-compile (require 'windmove))
(fset 'activate-windmove
      (lambda ()
        (unless (boundp 'windmove-wrap-around)
          (windmove-default-keybindings 'meta) ;; modifier-key = Alt
          (setq windmove-wrap-around t) ;; wrap-around を有効化
          (remove-hook 'window-configuration-change-hook 'activate-windmove) ;; 呼出し後 hook から削除
          (fmakunbound 'activate-windmove)))) ;; 呼出し後シンボルの関数ポインタを "void" にする
(add-hook 'window-configuration-change-hook 'activate-windmove)

;;------------------------------------------------------------------------------
;; vc-mode
;; バージョン管理
;;------------------------------------------------------------------------------
;; vcを起動しない
(setq vc-handled-backends nil)

;; vc 関係の hook 削除
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)
