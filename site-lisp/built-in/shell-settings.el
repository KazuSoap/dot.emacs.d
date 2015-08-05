;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Code:
;;------------------------------------------------------------------------------
;; shell
;; shell を有効化
;; emacs default
;;------------------------------------------------------------------------------

;; correct exec-path ( 先頭小文字 , 末尾の"/"なし )
(setq exec-path (mapcar #'directory-file-name exec-path))

(when (eq system-type 'windows-nt)
  ;; load environment value
  (cond ((load "shell_env" t)
		 (dolist (path (reverse (split-string (getenv "PATH") ";")))
		   (add-to-list 'exec-path (directory-file-name path))))
		(t (display-loading-error-message "shell_env")))

  ;; load fakecygpty setting
  (unless (load "custom-fakecygpty" t)
	(display-loading-error-message "custom-fakecygpty")))

;;; (M-! and M-| and compile.el)
(setq shell-file-name "bash")
(setq shell-command-switch "-c")
(defvar explicit-shell-file-name)
(setq explicit-shell-file-name shell-file-name)

;; shell バッファがカレントの際、動いている process の coding-system 設定を undecided に
(defun set-shell-buffer-process-coding-system ()
  (let ((process (get-buffer-process (buffer-name))))
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
(advice-add 'comint-output-filter :before #'set-shell-buffer-process-coding-system)

;;; password のミニバッファ入力
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
