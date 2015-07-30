;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; elscreen
;; ウィンドウ構成管理
;; from package
;;------------------------------------------------------------------------------

;; elscreen の起動
(cond ((autoload-if-found 'elscreen-start "elscreen" t)
	   (elscreen-start)

	   ;; プレフィクスキーはC-z
	   (defvar elscreen-prefix-key)
	   (setq elscreen-prefix-key (kbd "C-z"))

	   ;; tabの表示および幅の設定
	   (defvar elscreen-display-tab)
	   (setq elscreen-display-tab t)

	   ;; modelineへの番号表示
	   (defvar elscreen-display-screen-number)
	   (setq elscreen-display-screen-number nil)

	   ;; タブの先頭に[X]を表示しない
	   (defvar elscreen-tab-display-kill-screen)
	   (setq elscreen-tab-display-kill-screen nil)

	   ;; header-lineの先頭に[<->]を表示しない
	   (defvar elscreen-tab-display-control)
	   (setq elscreen-tab-display-control nil)

	   (custom-set-faces
		'(elscreen-tab-background-face ((t (:background "black")))) ;; header-line
		'(elscreen-tab-current-screen-face ((t (:background "black" :foreground "yellow")))) ;;current-screen
		'(elscreen-tab-other-screen-face ((t (:background "black" :foreground "Gray72")))))) ;;other-screen
	  (t (display-loading-error-message "auto-async-byte-compile")))

;;------------------------------------------------------------------------------
;; elscreen-separate-buffer-list
;; screenごとに独自のバッファリスト
;; from package
;;------------------------------------------------------------------------------

;; (cond ((autoload-if-found 'elscreen-separate-buffer-list-mode "elscreen-separate-buffer-list" t)
;; 	   (elscreen-separate-buffer-list-mode 1))
;; 	  (t (display-loading-error-message "elscreen-separate-buffer-list")))
