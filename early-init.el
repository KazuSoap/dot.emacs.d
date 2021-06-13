;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; windows-misc
;;------------------------------------------------------------------------------
(eval-when-compile
  (defconst msys-root
    (let* ((reg_hkcu_uninstall_key "\"HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\"")
           (reg_query_cmd (concat "reg query " reg_hkcu_uninstall_key " -v InstallLocation -s | findstr msys64")))
      (ignore-errors
        (expand-file-name
         (nth 3 (split-string (shell-command-to-string reg_query_cmd) " +\\|\n")))))
    "Get the installation location of \"msys2\"")

  ;; emacs.exeのmanifestを書き換えてデフォルトのActive Code Pageを
  ;; cp932からutf-8に変更することで以下のhackは不要になる
  ;; https://gist.github.com/trueroad/d309d1931100634c2cd1058a0620c663
  ;; (defmacro set-function-args-encode (arg-pos)
  ;;   "Set the character code of the parameter passed to the subprocess to cp932"
  ;;   `(lambda (args)
  ;;      (when (> (length args) ,arg-pos)
  ;;        ;; (message "%s" (cond ((stringp args) args) (t (format "%s" args))))
  ;;        (setf (nthcdr ,arg-pos args)
  ;;              (mapcar (lambda (arg)
  ;;                        (if (multibyte-string-p arg)
  ;;                            (encode-coding-string arg 'cp932)
  ;;                          arg))
  ;;                      (nthcdr ,arg-pos args))))
  ;;      args))

  (defmacro windows-nt-core ()
    "Windows Specific Settings"
    (when (eq system-type 'windows-nt)
      `(progn
         ;; Set environment variable
         (setenv "HOME" (concat ,(concat msys-root "/home/") user-login-name))
         (setenv "SHELL" ,(concat msys-root "/usr/bin/bash"))
         (setenv "MSYSTEM" "MINGW64")
         (or (getenv "SHLVL") (setenv "SHLVL" "0"))
         (setq shell-file-name ,(getenv "SHELL"))

         ;; coding-system
         ;; デフォルトの文字コードを設定
         ;; 指定される文字コードは以下の項目
         ;; ① ファイルを新規作成した場合のデフォルト
         ;; ② サブプロセスでの IO
         ;; ③ 他の項目が指定されていない場合のデフォルト値
         (prefer-coding-system 'utf-8-unix)

         (fset 'cygpath
               (lambda (&optional option path)
                 "cygpath for emacs lisp"
                 (when path
                   (with-temp-buffer
                     (call-process ,(concat msys-root "/usr/bin/cygpath") nil '(t nil) nil option path)
                     (unless (bobp)
                       (goto-char (point-min))
                       (buffer-substring-no-properties (point) (line-end-position)))))))

         ;; emacs.exeのmanifestを書き換えてデフォルトのActive Code Pageを
         ;; cp932からutf-8に変更することで以下のhackは不要になる
         ;; https://gist.github.com/trueroad/d309d1931100634c2cd1058a0620c663
         ;; (advice-add 'call-process-region :filter-args (set-function-args-encode 5))
         ;; (advice-add 'call-process :filter-args (set-function-args-encode 4))
         ;; (advice-add 'start-process :filter-args (set-function-args-encode 3))
         ))))
(windows-nt-core)

;;------------------------------------------------------------------------------
;; garbage collection
;;------------------------------------------------------------------------------
(setq gc-cons-threshold (* 128 1024 1024))

;;------------------------------------------------------------------------------
;; face & frame parameters
;;------------------------------------------------------------------------------
;; color-theme
(load-theme 'wheatgrass t)
(set-face-attribute 'mode-line nil :foreground "gray85" :background "#4a5459")
(set-face-attribute 'fringe nil :background "black")

;; (face-spec-set 'mode-line '((t :foreground "gray85" :background "#4a5459")))
;; (face-spec-set 'fringe '((t :background "black")))

;; (custom-set-faces
;;  '(mode-line ((t :foreground "gray85" :background "#4a5459")))
;;  '(fringe ((t :background "black"))))

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
        (font . "fontset-myricty")
        ))

;;------------------------------------------------------------------------------
;; global minor-mode
;;------------------------------------------------------------------------------
(tool-bar-mode -1) ; tool-bar-mode
;; (menu-bar-mode -1) ; menu-bar-mode (default)
(fringe-mode -1) ; fringe-mode
(column-number-mode 1) ; モードラインに列番号表示
;; (line-number-mode -1) ; モードラインに行番号表示 (default)
;; (size-indication-mode -1) ; モードラインにファイルサイズ表示 (default)
(delete-selection-mode 1) ; リージョン上書き

;;------------------------------------------------------------------------------
;; common-misc
;;------------------------------------------------------------------------------
;; "yes or no"を"y or n"に
(fset 'yes-or-no-p #'y-or-n-p)

;; C-hでBS, shift+C-hでHelp
;; (keyboard-translate ?\C-h ?\C-?) ; translate `C-h' to BS
;; (keyboard-translate ?\C-? ?\C-h)  ; translate BS to `C-h'

;; インデントは tab でなく 半角スペース
(setq-default indent-tabs-mode nil)

;; 双方向テキスト
(setq-default bidi-display-reordering nil) ;; 双方向テキスト可否
;; (setq-default bidi-paragraph-direction 'left-to-right) ;; テキスト方向を強制 (default)

;; デフォルトのメジャーモード
(setq-default major-mode #'text-mode)

;; *scratch* バッファのメジャーモード
;; (setq initial-major-mode #'fundamental-mode)

;; startup-message off
(setq inhibit-startup-screen t)

;; ファイルのフルパスをタイトルバーに表示
(setq frame-title-format '((:eval (if (buffer-file-name) "%f" "%b")) " - Emacs"))

;; beep音 off
(setq ring-bell-function #'ignore)

;; don't make BackUp file
(setq auto-save-default nil) ;; #*
(setq make-backup-files nil) ;; *.~

;; mouse scroll
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control))))

;;------------------------------------------------------------------------------
;; local functions
;;------------------------------------------------------------------------------
;;; my-revert-buffer-no-confirm ------------------------------------------------
;; バッファを再読み込み
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

;;; my-window-resizer ----------------------------------------------------------
;; window size を調整
;; http://d.hatena.ne.jp/khiker/20100119/window_resize
;; (fset 'my-window-resizer
;;       (lambda ()
;;         "Control window size and position."
;;         (interactive)
;;         (let ((dx (if (= (nth 0 (window-edges)) 0) 1 -1))
;;               (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
;;               action
;;               c)
;;           (catch 'end-flag
;;             (while t
;;               (setq action (read-key-sequence-vector (format "size[%dx%d]" (window-width) (window-height))))
;;               (setq c (aref action 0))
;;               (cond ((= c ?l) (enlarge-window dx t))
;;                     ((= c ?h) (shrink-window dx t))
;;                     ((= c ?j) (enlarge-window dy))
;;                     ((= c ?k) (shrink-window dy))
;;                     (t (let ((command (key-binding action)))
;;                          (when command (call-interactively command)))
;;                        (message "Quit")
;;                        (throw 'end-flag t))))))))
;; (global-set-key (kbd "C-c C-r") 'my-window-resizer)

;;------------------------------------------------------------------------------
;; local macros
;;------------------------------------------------------------------------------
;;; my-debug-message -----------------------------------------------------------
;; 特定の関数を実行された際、引数と出力、backtrace を出力
;; (eval-when-compile
;;   (defmacro my-debug-message (fun-name)
;;     `(progn
;;        (fset (quote ,(intern (format "add-%s" fun-name)))
;;              (lambda (f &rest args)
;;                (backtrace)
;;                ;; (message "(%s)before:%s" ,(format "%s" fun-name) (prin1-to-string args))
;;                (message "(%s)before:%s" ,(format "%s" fun-name) args)

;;                (let ((ret (apply f args)))
;;                    (message "(%s)after:%s" ,(format "%s" fun-name) ret)
;;                  ret)))
;;        (advice-add ',fun-name :around ',(intern (format "add-%s" fun-name)) '((depth . 100))))))

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
(eval-when-compile (require 'ediff))
(with-eval-after-load 'ediff
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
(eval-when-compile (require 'tramp))
(with-eval-after-load 'tramp
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
(eval-when-compile (require 'whitespace))
(with-eval-after-load 'whitespace
  ;; 保存時に行末の空白を削除する
  (add-hook 'before-save-hook #'delete-trailing-whitespace)

  ;; 可視化する不可視文字のリスト
  (setq whitespace-style '(face tabs tab-mark newline newline-mark spaces space-mark trailing))

  ;; 表示の変更
  (setq whitespace-display-mappings
        '(;; space → " "
          (space-mark   ?\xA0   [?\u00A4]     [?_])
          (space-mark   ?\x8A0  [?\x8A4]      [?_])
          (space-mark   ?\x920  [?\x924]      [?_])
          (space-mark   ?\xE20  [?\xE24]      [?_])
          (space-mark   ?\xF20  [?\xF24]      [?_])
          ;; full-width-space → "□"
          (space-mark   ?\u3000 [?\u25a1]     [?_ ?_])
          ;; tab → "»" with underline
          (tab-mark     ?\t     [?\xBB ?\t]   [?\\ ?\t])
          ;; newline → "｣"
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
