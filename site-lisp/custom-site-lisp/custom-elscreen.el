;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; custom-elscreen
;;------------------------------------------------------------------------------
;;; elscreen -------------------------------------------------------------------
;; ウィンドウ構成管理
;; from package

;; プレフィクスキーはC-z
(defvar elscreen-prefix-key)
(setq elscreen-prefix-key (kbd "C-z"))
(elscreen-start)

;; tabの表示および幅の設定
(defvar elscreen-display-tab)
(setq elscreen-display-tab t)

;; modelineへの番号表示
(defvar elscreen-display-screen-number)
(setq elscreen-display-screen-number nil)

;;; タブの先頭に[X]を表示しない
(defvar elscreen-tab-display-kill-screen)
(setq elscreen-tab-display-kill-screen nil)

;;; header-lineの先頭に[<->]を表示しない
(defvar elscreen-tab-display-control)
(setq elscreen-tab-display-control nil)

(custom-set-faces
 '(elscreen-tab-background-face ((t (:background "black")))) ;; header-line
 ;'(elscreen-tab-control-face ((t (:background "Gray95")))) ;; tab-control
 '(elscreen-tab-current-screen-face ((t (:background "black" :foreground "yellow")))) ;;current-screen
 '(elscreen-tab-other-screen-face ((t (:background "black" :foreground "Gray72")))) ;;other-screen
 )

;;; elscreen-separate-buffer-list ----------------------------------------------
;; screenごとに独自のバッファリスト
;; from package
(elscreen-separate-buffer-list-mode 1)
