;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; shell
;; shell を有効化
;; emacs default
;;------------------------------------------------------------------------------

(setq shell-command-switch "-c")

(defvar exec-path-updated
  (let ((shell-level (getenv "SHLVL")))
    (if (or (not shell-level) (string= "0" shell-level)) nil t)))

(defmacro set-exec-path-before-mkproc (fun-name)
  `(progn
     (defun ,(intern (format "ad-%s" fun-name)) (&rest args)
       (when (not exec-path-updated)
         (setq exec-path-updated t)
         (exec-path-from-shell-copy-envs '("PATH" "MANPATH" "PKG_CONFIG_PATH" "LANG")))
       args)
     (advice-add ',fun-name :before ',(intern (format "ad-%s" fun-name)))))

(unless exec-path-updated
  (set-exec-path-before-mkproc shell)
  (set-exec-path-before-mkproc executable-find)
  (set-exec-path-before-mkproc call-process)
  (set-exec-path-before-mkproc call-process-region)
  (set-exec-path-before-mkproc start-process)
  (set-exec-path-before-mkproc make-process))

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

;; key-bind
(global-set-key [f8] 'shell-pop)
