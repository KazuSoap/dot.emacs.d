;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; win-misc
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; local functions & macro
;;------------------------------------------------------------------------------
(defun cygpath (&optional option path)
  "cygpath for emacs lisp"
  (if path (with-temp-buffer
             (call-process "d:/msys64/usr/bin/cygpath" nil '(t nil) nil option path)
             (unless (bobp)
               (goto-char (point-min))
               (buffer-substring-no-properties (point) (line-end-position))))))

;;------------------------------------------------------------------------------
;; IME
;;------------------------------------------------------------------------------
;;-- IME customize --;;
;; IME ON/OFF 時のカーソルカラー設定用関数
(defun w32-ime-on-hooks () (set-cursor-color "yellow"))
(defun w32-ime-off-hooks () (set-cursor-color "thistle"))

;; IMEのカスタマイズ
(setq default-input-method "W32-IME") ;;標準IMEの設定

;; Windows IME の ON:[あ]/OFF:[Aa] をモードラインに表示
(setq-default w32-ime-mode-line-state-indicator "[Aa]")
(setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

;; IME の初期化
(w32-ime-initialize)

;; IME ON/OFF時のカーソルカラー
(add-hook 'w32-ime-on-hook 'w32-ime-on-hooks)
(add-hook 'w32-ime-off-hook 'w32-ime-off-hooks)

;; IMEの制御(yes/noをタイプするところでは IME をオフにする)
(wrap-function-to-control-ime 'universal-argument t nil)
(wrap-function-to-control-ime 'read-string nil nil)
(wrap-function-to-control-ime 'read-char nil nil)
(wrap-function-to-control-ime 'read-from-minibuffer nil nil)
(wrap-function-to-control-ime 'y-or-n-p nil nil)
(wrap-function-to-control-ime 'yes-or-no-p nil nil)
(wrap-function-to-control-ime 'map-y-or-n-p nil nil)
