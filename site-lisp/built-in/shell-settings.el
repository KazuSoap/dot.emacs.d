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

(when (eq system-type 'windows-nt)
  ;; emacs-24.4 or later において、4096 byte を超えるデータを一度に pipe 経由で
  ;; サブプロセスに送り込むと、レスポンスが帰ってこなくなる。
  ;; -> 4096 byte ごとにデータを区切って pipe に送るようにする。
  ;; 参考：2ch NTEmacsスレッド４ 699 以降
  (defun ad-process-send-string (orig-fun &rest args)
    (if (not (eq (process-type (car args)) 'real))
        (apply orig-fun args)
      (let* ((process (or (car args)
                          (get-buffer-process (current-buffer))))
             (send-string (encode-coding-string (nth 1 args)
                                                (cdr (process-coding-system (get-process process)))))
             (send-string-length (length send-string)))
        (let ((w32-pipe-limit 4096)
              (inhibit-eol-conversion t)
              (from 0)
              to)
          (while (< from send-string-length)
            (setq to (min (+ from w32-pipe-limit) send-string-length))
            (setf (nth 1 args) (substring send-string from to))
            (apply orig-fun args)
            (setq from to))))))
  (advice-add 'process-send-string :around 'ad-process-send-string))

;;; password のミニバッファ入力
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
