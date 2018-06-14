;;; -*- coding: utf-8; lexical-binding: t -*-

;;==============================================================================
;; emacs built in
;;==============================================================================
;;------------------------------------------------------------------------------
;; cua-mode
;; C-Ret で矩形選択
;;------------------------------------------------------------------------------
;; 詳しいキーバインド操作：http://dev.ariel-networks.com/articles/emacs/part5/
(with-eval-after-load 'cua-base
  (setq-default cua-enable-cua-keys nil))

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
;; eldoc
;;
;;------------------------------------------------------------------------------
(with-eval-after-load 'eldoc
  (setq-default eldoc-idle-delay 0.5)) ;; eldoc 遅延

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
;; auto-insert
;; ファイルの種類に応じたテンプレートの挿入
;;------------------------------------------------------------------------------

(with-eval-after-load 'autoinsert
  ;; テンプレートのディレクトリ
  (setq-default auto-insert-directory (eval-when-compile (expand-file-name (concat user-emacs-directory "auto-insert"))))

  ;; テンプレート中で展開してほしいテンプレート変数を定義
  (setq-default template-replacements-alists
                `(("%file%"             . ,(lambda () (file-name-nondirectory (buffer-file-name))))
                  ("%file-without-ext%" . ,(lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
                  ("%include-guard%"    . ,(lambda () (format "INCLUDE_%s_H" (upcase (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))))))

  (fset 'my-template
        (lambda ()
          (time-stamp)
          (dolist (c (default-value 'template-replacements-alists))
            (goto-char (point-min))
            (while (search-forward (car c) nil t)
              (replace-match (funcall (cdr c)))))
          (goto-char (point-max))
          (message "done.")))

  ;; 各ファイルによってテンプレートを切り替える
  (defvar auto-insert-alist)
  (add-to-list 'auto-insert-alist '("\\.cpp$"   . ["template.cpp" my-template]))
  (add-to-list 'auto-insert-alist '("\\.h$"     . ["template.h" my-template]))
  (add-to-list 'auto-insert-alist '("Makefile$" . ["template.make" my-template])))

(add-hook 'find-file-not-found-hooks #'auto-insert)

;;------------------------------------------------------------------------------
;; whitespace-mode
;; 不可視文字の可視化
;;------------------------------------------------------------------------------
(with-eval-after-load 'whitespace
  ;; 保存時に行末の空白を削除する
  (add-hook 'before-save-hook #'delete-trailing-whitespace)

  (setq-default whitespace-style '(face tabs tab-mark newline newline-mark spaces space-mark trailing))

  ;; 表示の変更
  (setq-default whitespace-display-mappings
        '(;; space → " "
          (space-mark ?\xA0 [?\u00A4] [?_])
          (space-mark ?\x8A0 [?\x8A4] [?_])
          (space-mark ?\x920 [?\x924] [?_])
          (space-mark ?\xE20 [?\xE24] [?_])
          (space-mark ?\xF20 [?\xF24] [?_])
          ;; full-width-space → "□"
          (space-mark ?\u3000 [?\u25a1] [?_ ?_])
          ;; tab → "»" with underline
          (tab-mark     ?\t    [?\xBB ?\t]   [?\\ ?\t])
          ;; newline → "｣"
          (newline-mark ?\n    [?\uFF63 ?\n] [?$ ?\n])))

  ;; 以下の正規表現にマッチするものを"space"と認識
  (setq-default whitespace-space-regexp "\\(\u3000+\\)"))

;;------------------------------------------------------------------------------
;; vc-mode
;; バージョン管理
;;------------------------------------------------------------------------------
;; vcを起動しない
(setq-default vc-handled-backends nil)

;; vc 関係の hook 削除
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)

;;==============================================================================
;; from package
;;==============================================================================
;;------------------------------------------------------------------------------
;; company
;; 補完システム
;;------------------------------------------------------------------------------
(with-eval-after-load 'company
  ;; 基本設定
  (setq-default company-idle-delay nil) ;; 遅延
  (setq-default company-minimum-prefix-length 3) ;; 補完開始文字長
  (setq-default company-selection-wrap-around t) ;; 最下時に↓で最初に戻る
  (define-key (default-value 'company-mode-map) (kbd "C-<tab>") 'company-complete)) ;; 補完は手動

;;------------------------------------------------------------------------------
;; elscreen
;; ウィンドウ構成管理
;;------------------------------------------------------------------------------
(with-eval-after-load 'elscreen
  (setq-default elscreen-display-tab t) ;; tabの表示および幅の設定
  (setq-default elscreen-display-screen-number nil) ;; modelineへの番号表示
  (setq-default elscreen-tab-display-kill-screen nil) ;; タブの先頭に[X]を表示しない
  (setq-default elscreen-tab-display-control nil)) ;; header-lineの先頭に[<->]を表示しない

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

  (declare-function flycheck-add-mode "flycheck")
  (flycheck-add-mode 'emacs-lisp 'elisp-mode))

;;------------------------------------------------------------------------------
;; helm
;; emacsに統一的なある”操作方式”を提供するフレームワーク
;;------------------------------------------------------------------------------
(with-eval-after-load 'helm
  ;; helm-migemo を有効化
  (require 'cmigemo)
  (declare-function helm-migemo-mode "helm-multi-match")
  (helm-migemo-mode)

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
;; irony
;; A C/C++ minor mode powered by libclang
;;------------------------------------------------------------------------------
(with-eval-after-load 'irony
  ;; irony-server-install に失敗する問題の修正
  ;; コンパイラに clang を指定
  (fset 'ad-irony--install-server-read-command
        (lambda (args)
          "modify irony--install-server-read-command"
          ;; (setenv "CC" "clang") (setenv "CXX" "clang++")
          `(,(replace-regexp-in-string
              (format "^\\(%s\\)" (shell-quote-argument (default-value 'irony-cmake-executable)))
              (default-value 'ex-irony--install-server-read-cmd)
              (car args)))))
  (advice-add 'irony--install-server-read-command :filter-args 'ad-irony--install-server-read-command)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))

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
(advice-add 'isearch-mode :before (lambda (&rest args) (require 'cmigemo) args))

;;------------------------------------------------------------------------------
;; nlinum-mode
;;
;;------------------------------------------------------------------------------
(with-eval-after-load 'nlinum
  (defvar nlinum-format)

  (fset 'dynamic-default-nlinum-width
        (lambda ()
          (set (make-local-variable 'nlinum-format)
               (concat "%" (number-to-string
                            ;; estimate max digit number of buffer lines.
                            (max 2 (1+ (floor (log (max 1 (count-lines 1 (point-max))) 10)))))
                       "d\u007c"))))
  (add-hook 'nlinum-mode-on-hook 'dynamic-default-nlinum-width))

;;------------------------------------------------------------------------------
;; plantuml-mode
;;
;;------------------------------------------------------------------------------
(with-eval-after-load 'plantuml-mode
  (setq-default plantuml-jar-path "c:/msys64/usr/local/share/plantuml/plantuml.jar")

  ;; javaにオプションを渡したい場合はここにかく
  (setq-default plantuml-java-options "")

  ;; plantumlのプレビュー出力形式(svg,png,txt,utxt)
  ;; (setq-default plantuml-output-type "txt")

  ;; 日本語を含むUMLを書く場合はUTF-8を指定
  (setq-default plantuml-options "-charset UTF-8"))

;; 拡張子による major-mode の関連付け
(add-to-list 'auto-mode-alist '("\\.puml$" . plantuml-mode))

;;------------------------------------------------------------------------------
;; smert compile
;;
;;------------------------------------------------------------------------------
(with-eval-after-load 'smart-compile
  (defvar smart-compile-alist)
  (add-to-list 'smart-compile-alist '("\\.puml$" . "plantuml -charset UTF-8 -tsvg %f") t)
  (add-to-list 'smart-compile-alist '(elisp-mode emacs-lisp-byte-compile)))
(global-set-key (kbd "C-x c") #'smart-compile)

;;------------------------------------------------------------------------------
;; vs-set-c-style
;; Visual Studio スタイルのインデント設定
;; http://yohshiy.blog.fc2.com/blog-entry-264.html
;;------------------------------------------------------------------------------
(autoload 'vs-set-c-style "vs-set-c-style")
