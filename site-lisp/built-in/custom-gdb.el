;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; GDB
;; デバッガ
;; built in
;;------------------------------------------------------------------------------

;;; 有用なバッファを開くモード
(defvar gdb-many-windows)
(setq gdb-many-windows t)

;;; 変数の上にマウスカーソルを置くと値を表示
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))

;;; I/O バッファを表示
(defvar gdb-use-separate-io-buffer)
(setq gdb-use-separate-io-buffer t)

;;; t にすると mini buffer に値が表示される
(defvar gud-tooltip-echo-area)
(setq gud-tooltip-echo-area nil)
