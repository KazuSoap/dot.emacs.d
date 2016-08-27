;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; shell
;; shell を有効化
;; emacs default
;;------------------------------------------------------------------------------

(setq shell-command-switch "-c")

(let ((shell-level (getenv "SHLVL")))
  (when (or (not shell-level) (string= "0" shell-level))
    (when (eq system-type 'windows-nt)
      (setq shell-file-name "d:/msys64/usr/bin/bash")
      (defvar explicit-shell-file-name)
      (setq explicit-shell-file-name shell-file-name)
      (setenv "SHELL" shell-file-name)
      (setenv "MSYSTEM" "MINGW64")

      (defun ad-exec-path-from-shell-setenv (orig-fun &rest args)
        (when (and (string= (car args) "PATH") (fboundp 'cygpath))
          (setf (nth 1 args) (cygpath "-amp" (nth 1 args))))
        (apply orig-fun args))
      (advice-add 'exec-path-from-shell-setenv :around 'ad-exec-path-from-shell-setenv))

    (exec-path-from-shell-copy-envs '("PATH" "MANPATH" "PKG_CONFIG_PATH" "LANG"))))

;;; password のミニバッファ入力
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
