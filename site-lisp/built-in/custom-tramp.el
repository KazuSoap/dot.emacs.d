;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;;; TRAMP(TransparentRemoteAccessMultipleProtocol) -----------------------------
;; emacsからリモートファイルを操作
;; emacs default
;;------------------------------------------------------------------------------

(with-eval-after-load 'tramp
  (defvar tramp-default-method)
  (setq tramp-default-method "scp")
  (defvar tramp-encoding-shell)
  (setq tramp-encoding-shell "bash")

  ;; リモートサーバで shell を開いた時に日本語が文字化けしないよう、LC_ALL の設定を無効にする
  ;; http://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-processes.html#Running%20a%20debugger%20on%20a%20remote%20host
  (defvar tramp-remote-process-environment)
  (let ((process-environment tramp-remote-process-environment))
	(setenv "LC_ALL" nil)
	(setq tramp-remote-process-environment process-environment)))
