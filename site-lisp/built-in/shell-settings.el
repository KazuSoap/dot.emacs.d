;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; shell
;; shell を有効化
;; emacs default
;;------------------------------------------------------------------------------

;;; (M-! and M-| and compile.el)
;; (setq shell-file-name "bash")
(setq shell-file-name (directory-file-name "D:/msys64/usr/bin/bash"))
(setq shell-command-switch "-c")
(defvar explicit-shell-file-name)
(setq explicit-shell-file-name shell-file-name)

(when (and (eq system-type 'windows-nt) (not (getenv "MSYSTEM")))
  (defun ad-exec-path-from-shell-setenv (orig-fun &rest args)
	(when (string=  (nth 0 args) "PATH")
	  (let (win_path)
		(dolist (path (split-string (nth 1 args) ":"))
		  (setq win_path (concat win_path (cygwin-mount-substitute-longest-mount-name path) ";")))
		(setcar (nthcdr 1 args) win_path)))
	(apply orig-fun args))
  (advice-add 'exec-path-from-shell-setenv :around 'ad-exec-path-from-shell-setenv)

  (setenv "SHELL" shell-file-name)
  (setenv "MSYSTEM" "MINGW64")

  (let ((envs '("PATH" "MANPATH" "PKG_CONFIG_PATH")))
	(exec-path-from-shell-copy-envs envs))

  ;; load fakecygpty setting
  (load "custom-fakecygpty" t))

;; shell バッファがカレントの際、動いている process の coding-system 設定を undecided に
(defun set-shell-buffer-process-coding-system (&rest args)
  (let ((process (car args)))
	(if (and process
			 (string-match "^shell" (process-name process)))
		(let ((coding-system (process-coding-system process)))
		  (set-process-coding-system process
									 (coding-system-change-text-conversion
									  (car coding-system) 'undecided)
									 (cdr coding-system))))))

;; shellバッファで、コマンド実行結果出力前に set-shell-buffer-process-coding-system を実行する。
;; この設定により、shellバッファで utf-8 の出力をする cygwin コマンドと、cp932 の出力をする
;; Windowsコマンドの漢字の文字化けが回避される。また、漢字を含むプロンプトが文字化けする場合には、
;; .bashrc の PS1 の設定の後に「export PS1="$(sleep 0.1)$PS1"」を追加すれば、回避できる模様。
(advice-add 'comint-output-filter :before 'set-shell-buffer-process-coding-system)

;;; password のミニバッファ入力
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
