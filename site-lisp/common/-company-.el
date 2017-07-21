;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; company
;; 補完システム
;; from package
;;------------------------------------------------------------------------------

;; 不要な backend を削除
(custom-set-variables
 '(company-backends
   '( company-bbdb
      company-nxml
      company-css
      ;; company-eclim
      company-semantic
      ;; company-clang
      ;; company-xcode
      ;; company-cmake
      company-capf
      company-files
      ( company-dabbrev-code
        ;; company-gtags
        company-etags
        company-keywords )
      company-oddmuse
      company-dabbrev)))

(with-eval-after-load 'company
  ;; 基本設定
  (defvar company-idle-delay)  ;; 遅延
  (setq company-idle-delay nil)
  (defvar company-mode-map)
  (define-key company-mode-map (kbd "C-<tab>") 'company-complete) ;; 補完は手動
  (defvar company-minimum-prefix-length) ;; 補完開始文字長
  (setq company-minimum-prefix-length 3)
  (defvar company-selection-wrap-around)  ;; 最下時に↓で最初に戻る
  (setq company-selection-wrap-around t))
