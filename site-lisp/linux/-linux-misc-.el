;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; linux-misc
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; IME
;;------------------------------------------------------------------------------
;; anthy のロード
(load "leim-list")
(require 'anthy)

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
