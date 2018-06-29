;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; garbage collection
;;------------------------------------------------------------------------------
(setq gc-cons-threshold (* 128 1024 1024))

;;------------------------------------------------------------------------------
;; HOME
;;------------------------------------------------------------------------------
(setenv "HOME" "d:/Users/USERNAME")

;;------------------------------------------------------------------------------
;; global minor-mode
;;------------------------------------------------------------------------------
(column-number-mode) ;; モードラインに列番号表示 (default off)
;; (line-number-mode -1) ;; モードラインに行番号表示 (default on)
;; (size-indication-mode -1) ;; モードラインにファイルサイズ表示 (default off)

;;------------------------------------------------------------------------------
;; common-misc
;;------------------------------------------------------------------------------
;; デフォルトの文字コードを設定
;; 指定される文字コードは以下の項目
;; ① ファイルを新規作成した場合のデフォルト
;; ② サブプロセスでの IO
;; ③ 他の項目が指定されていない場合のデフォルト値
(prefer-coding-system 'utf-8-unix)

;; サブプロセスが出力する文字コードを判定して、process-coding-system の設定値を決定
;; (サブプロセスの "input-coding" を "undecided" にして実現)
(setq-default default-process-coding-system
              `(,(eval-when-compile
                   (let* ((input-coding (car (default-value 'default-process-coding-system)))
                          (input-eol-type (coding-system-eol-type input-coding)))
                     (cond ((eq input-eol-type 0) 'undecided-unix)
                           ((eq input-eol-type 1) 'undecided-dos)
                           ((eq input-eol-type 2) 'undecided-mac)
                           (t input-coding))))
                . utf-8-unix))

;; color-them
(load-theme 'wheatgrass t)
(set-face-attribute 'mode-line nil :foreground "gray85" :background "dark slate gray") ;; mode line in active
(set-face-attribute 'fringe nil :background "black")
(with-eval-after-load 'display-line-numbers
  (set-face-attribute 'line-number nil :background "gray10")
  (set-face-attribute 'line-number-current-line nil :background "gray40"))

;; "yes or no"を"y or n"に
(fset 'yes-or-no-p #'y-or-n-p)

;; C-hでBS, shift+C-hでHelp
(keyboard-translate ?\C-h ?\C-?) ; translate `C-h' to DEL
(keyboard-translate ?\C-? ?\C-h)  ; translate DEL to `C-h'

;; 分割したウィンドウ間を Alt + 矢印キー で移動
(windmove-default-keybindings 'meta)
(setq-default windmove-wrap-around t) ;; window 間移動を環状にする

;; ファイルのフルパスをタイトルバーに表示
(setq-default frame-title-format (format "%%f - Emacs"))

;; beep音 off
(setq-default ring-bell-function #'ignore)

;; don't make BackUp file
(setq-default auto-save-default nil) ;; #*
(setq-default make-backup-files nil) ;; *.~

;; インデントは tab でなく 半角スペース
(setq-default indent-tabs-mode nil)

;; 双方向テキスト
(setq-default bidi-display-reordering nil) ;; 双方向テキスト可否
(setq-default bidi-paragraph-direction 'left-to-right) ;; テキスト方向を強制

;; mouse scroll
(setq-default mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control))))

;; uniquify
(setq-default uniquify-buffer-name-style 'post-forward-angle-brackets) ;; 表示形式指定
(setq-default uniquify-ignore-buffers-re "*[^*]+*") ;; 無視するバッファ名

;;------------------------------------------------------------------------------
;; custom-set-*
;;------------------------------------------------------------------------------
;; custom-save-all された直後に custom-file をバイトコンパイル
;; (declare-function custom-file "cus-edit")
;; (fset 'ad-custom-save-all (lambda() (byte-compile-file (custom-file) t)))
;; (advice-add 'custom-save-all :after 'ad-custom-save-all)
