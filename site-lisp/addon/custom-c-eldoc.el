;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; c-eldoc
;; eldoc の C 言語拡張
;; from package
;;------------------------------------------------------------------------------

;; C C++ で自動的に有効化
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook 'c-turn-on-eldoc-mode))

(defun which_cpp ()
  "which cpp command"
	(substring (directory-file-name (shell-command-to-string "which cpp")) 0 -1))

(with-eval-after-load 'c-eldoc
  (defvar c-eldoc-buffer-regenerate-time)
  (setq c-eldoc-buffer-regenerate-time 60)
  (defvar c-eldoc-cpp-command)
  (let ((cpp_path (which_cpp)))
  (setq c-eldoc-cpp-command cpp_path))
  (defvar c-eldoc-cpp-macro-arguments)
  (setq c-eldoc-cpp-macro-arguments "-dD -w -P")
  (defvar c-eldoc-cpp-normal-arguments)
  (setq c-eldoc-cpp-normal-arguments "-w -P")
  (defvar c-eldoc-includes)
  (setq c-eldoc-includes "`pkg-config --cflags opencv` -I./ -I../ "))
