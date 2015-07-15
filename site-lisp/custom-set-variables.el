;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; custom-set-variables
;;------------------------------------------------------------------------------

;;-- ファイルのフルパスをタイトルバーに表示 --;;
(setq frame-title-format(format "%%f - Emacs"))

;;-- "yes or no"を"y or n"に --;;
(fset 'yes-or-no-p 'y-or-n-p)

;;-- 非表示,消音 --;;
(setq inhibit-startup-screen t) ;; startup-message
(setq ring-bell-function 'ignore) ;; beep音,画面flash

;;-- don't make BackUp file --;;
(setq auto-save-default nil) ;; #*
(setq make-backup-files nil) ;; *.~

;;-- C-Ret で矩形選択 --;;
;; 詳しいキーバインド操作：http://dev.ariel-networks.com/articles/emacs/part5/
(with-eval-after-load 'cua-base
  (defvar cua-enable-cua-keys)
  (setq cua-enable-cua-keys nil))

;;-- 右から左に書く言語のための設定を無効化 --;;
(setq bidi-display-reordering nil)
(setq bidi-paragraph-direction (quote right-to-left))

;;-- vcを起動しないようにする --;;
(setq vc-handled-backends nil)
