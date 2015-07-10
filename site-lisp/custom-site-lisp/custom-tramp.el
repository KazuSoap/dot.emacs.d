;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; tramp 設定
;;------------------------------------------------------------------------------
;;; TRAMP(TransparentRemoteAccessMultipleProtocol) -----------------------------
;; emacsからリモートファイルを操作
;; emacs default
(defvar tramp-default-method)
(setq tramp-default-method "scp")
(defvar tramp-encoding-shell)
(setq tramp-encoding-shell "bash")

;; リモートサーバで shell を開いた時に日本語が文字化けしないよう、LC_ALL の設定を無効にする
;; http://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-processes.html#Running%20a%20debugger%20on%20a%20remote%20host
(defvar tramp-remote-process-environment)
(let ((process-environment tramp-remote-process-environment))
  (setenv "LC_ALL" nil)
  (setq tramp-remote-process-environment process-environment))

;; ドライブレターの後の「：」が tramp-method の後の「：」と混同されるのを対策する
(defadvice tramp-do-copy-or-rename-file-out-of-band (around ad-tramp-do-copy-or-rename-file-out-of-band activate)
  (let ((default-directory "/"))
	(unless (tramp-tramp-file-p (ad-get-arg 1))
	  (ad-set-arg 1 (substring (shell-command-to-string
								(concat "cygpath -u " (shell-quote-argument (ad-get-arg 1))))
							   0 -1)))
	(unless (tramp-tramp-file-p (ad-get-arg 2))
	  (ad-set-arg 2 (substring (shell-command-to-string
								(concat "cygpath -u " (shell-quote-argument (ad-get-arg 2))))
							   0 -1))))
  ad-do-it
  (sit-for 0.1)) ; delay(NTEmacs64 では必要)
