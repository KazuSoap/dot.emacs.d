;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; general key bind
;;------------------------------------------------------------------------------

;; C-hでBS, shift+C-hでHelp
(keyboard-translate ?\C-h ?\C-?) ; translate `C-h' to DEL
(keyboard-translate ?\C-? ?\C-h)  ; translate DEL to `C-h'

(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm) ; reload buffer
(global-set-key (kbd "C-c C-r") 'my-window-resizer)

;; smart-compile
(global-set-key (kbd "C-x c") 'smart-compile)
