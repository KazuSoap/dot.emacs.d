;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; linux-misc
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; IME
;;------------------------------------------------------------------------------

;; emacs-mozc
(require 'mozc)
(setq default-input-method "japanese-mozc")

(require 'mozc-popup)
(setq mozc-candidate-style 'popup)

(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)

;;------------------------------------------------------------------------------
;; set-faces
;;------------------------------------------------------------------------------
(when (not window-system)
  ;;-- mode line in active --;;
  (set-face-attribute 'mode-line nil :foreground "white" :background "DarkRed")

  ;;-- linum-mode --;;
  (with-eval-after-load 'linum
    (set-face-attribute 'linum nil :foreground "gray" :background "black" :underline nil))

  ;;-- whitespace --;;
  (with-eval-after-load 'whitespace
    ;; space
    (set-face-attribute 'whitespace-space nil :foreground "GreenYellow" :background "black")
    ;; tab
    (set-face-attribute 'whitespace-tab nil :foreground "LightSkyBlue" :background "black" :underline t)
    ;; newline
    (set-face-attribute 'whitespace-newline nil :foreground "DeepSkyBlue")
    ;; trailing
    (set-face-attribute 'whitespace-trailing nil :background "DeepPink"))

  ;;-- elscreen --;;
  (with-eval-after-load 'elscreen
    ;; tab-background (header-line)
    (set-face-attribute 'elscreen-tab-background-face nil :background "black" :underline nil)
    ;; tab-current-screen (current-screen)
    (set-face-attribute 'elscreen-tab-current-screen-face nil :foreground "yellow" :background "black" :underline nil)
    ;; tab-other-screen (other-screen)
    (set-face-attribute 'elscreen-tab-other-screen-face nil :foreground "Gray72" :background "black" :underline nil)))

;;------------------------------------------------------------------------------
;; misc
;;------------------------------------------------------------------------------
(setq select-enable-clipboard t)

;;------------------------------------------------------------------------------
;; irony
;; A C/C++ minor mode powered by libclang
;; from package
;;------------------------------------------------------------------------------

(with-eval-after-load 'irony
  ;; irony-server-install に失敗する問題の修正用
  (defvar ex-irony--install-server-read-cmd "\\1 -DLIBCLANG_LIBRARY=/usr/lib/llvm-3.8/lib/libclang.so"))