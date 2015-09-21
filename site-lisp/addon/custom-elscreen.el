;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; elscreen
;; ウィンドウ構成管理
;; from package
;;------------------------------------------------------------------------------

(elscreen-start) ;; elscreen の起動

(defvar elscreen-display-tab) ;; tabの表示および幅の設定
(setq elscreen-display-tab t)

(defvar elscreen-display-screen-number) ;; modelineへの番号表示
(setq elscreen-display-screen-number nil)

(defvar elscreen-tab-display-kill-screen) ;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)

(defvar elscreen-tab-display-control) ;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)

;;------------------------------------------------------------------------------
;; elscreen-separate-buffer-list
;; screenごとに独自のバッファリスト
;; from package
;;------------------------------------------------------------------------------

;; (autoload 'elscreen-separate-buffer-list-mode "elscreen-separate-buffer-list" t)
;; (elscreen-separate-buffer-list-mode 1)
