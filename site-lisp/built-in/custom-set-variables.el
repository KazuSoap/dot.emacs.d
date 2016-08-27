;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; custom-set-variables
;;------------------------------------------------------------------------------

;; color-theme
(load-theme 'wheatgrass t)

;; ファイルのフルパスをタイトルバーに表示
(setq frame-title-format(format "%%f - Emacs"))

;; "yes or no"を"y or n"に
(fset 'yes-or-no-p 'y-or-n-p)

;; 非表示,消音
(setq inhibit-startup-screen t) ;; startup-message
(setq ring-bell-function 'ignore) ;; beep音,画面flash

;; don't make BackUp file
(setq auto-save-default nil) ;; #*
(setq make-backup-files nil) ;; *.~

;; インデントは tab でなく 半角スペース
(setq-default indent-tabs-mode nil)

;; C-Ret で矩形選択
;; 詳しいキーバインド操作：http://dev.ariel-networks.com/articles/emacs/part5/
(with-eval-after-load 'cua-base
  (defvar cua-enable-cua-keys)
  (setq cua-enable-cua-keys nil))

;; 分割したウィンドウ間を Shift + 矢印キー で移動
(windmove-default-keybindings 'meta)
(defvar windmove-wrap-around)
(setq windmove-wrap-around t)

;; 右から左に書く言語のための設定を無効化
(setq bidi-display-reordering nil)
(setq bidi-paragraph-direction (quote right-to-left))

;; vcを起動しないようにする
(setq vc-handled-backends nil)

;; kkc による日本語入力で「nn」→「ん」に変換
(defvar quail-japanese-use-double-n nil)
(setq quail-japanese-use-double-n t)

;; デフォルトの文字コードを設定
;; 指定される文字コードは以下の項目
;; ① ファイルを新規作成した場合のデフォルト
;; ② サブプロセスでの IO
;; ③ 他の項目が指定されていない場合のデフォルト値
(prefer-coding-system 'utf-8-unix)
