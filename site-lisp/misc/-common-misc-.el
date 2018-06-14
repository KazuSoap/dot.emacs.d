;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; global minor-mode
;;------------------------------------------------------------------------------
(menu-bar-mode) ;; 端末の場合 <F10> で menu 操作可能
(line-number-mode) ;; モードラインに行番号表示
(column-number-mode) ;; モードラインに列番号表示
(size-indication-mode -1) ;; モードラインにファイルサイズ表示
;; (blink-cursor-mode -1) ;; cursor点滅表示

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

;; startup-message off
(setq-default inhibit-startup-screen t)

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
(declare-function custom-file "cus-edit")
(fset 'ad-custom-save-all (lambda() (byte-compile-file (custom-file) t)))
(advice-add 'custom-save-all :after 'ad-custom-save-all)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet smart-compile nlinum migemo magit helm ggtags flycheck-plantuml flycheck-irony exec-path-from-shell esup elscreen company-irony-c-headers company-irony))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
