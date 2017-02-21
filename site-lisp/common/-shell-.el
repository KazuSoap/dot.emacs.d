;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; shell
;; shell を有効化
;; emacs default
;;------------------------------------------------------------------------------

(setq shell-command-switch "-c")

(let ((shell-level (getenv "SHLVL")))
  (when (or (not shell-level) (string= "0" shell-level))
    (exec-path-from-shell-copy-envs '("PATH" "MANPATH" "PKG_CONFIG_PATH" "LANG"))))

;;; password のミニバッファ入力
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;;------------------------------------------------------------------------------
;; TRAMP(TransparentRemoteAccessMultipleProtocol)
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

;;------------------------------------------------------------------------------
;; shell-pop
;; シェルバッファをポップアップ
;; from : package system
;;------------------------------------------------------------------------------
(with-eval-after-load 'shell-pop
  (defvar shell-pop-internal-mode)
  (setq shell-pop-internal-mode "shell")

  (defvar shell-pop-internal-mode-buffer)
  (setq shell-pop-internal-mode-buffer "*shell-pop: shell*")

  (defvar shell-pop-internal-mode-func)
  (setq shell-pop-internal-mode-func (lambda () (shell)))

  (defvar shell-pop-window-size)
  (setq shell-pop-window-size 30)

  (defvar shell-pop-full-span)
  (setq shell-pop-full-span t)

  (defvar shell-pop-window-position)
  (setq shell-pop-window-position "bottom"))