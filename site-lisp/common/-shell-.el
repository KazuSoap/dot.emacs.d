;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; shell
;;------------------------------------------------------------------------------
(eval-when-compile
  (defconst my-shell_env-file (concat user-emacs-directory "site-lisp/-shell_env-.el")))

(when (string= "0" (getenv "SHLVL"))
  (cond ((and (load "-shell_env-" t t)
              (time-less-p (nth 5 (file-attributes "~/.bash_profile"))
                           (nth 5 (file-attributes (eval-when-compile my-shell_env-file)))))
         (setq-default explicit-shell-file-name (setq shell-file-name (getenv "SHELL"))))
        (t ;; else
         (let ((env_val_lst '("SHELL" "PATH" "MANPATH" "PKG_CONFIG_PATH" "LANG" "JAVA_HOME" "GRAPHVIZ_DOT"))
               (shell_env nil))
           ;; sync emacs environment value with shell's one
           (exec-path-from-shell-copy-envs env_val_lst)

           ;; save sync environment value
           (dolist (val env_val_lst)
             (setq shell_env (concat shell_env (format "(setenv \"%s\" \"%s\")\n" val (getenv val)))))

           (with-temp-buffer
             (insert shell_env "(setq exec-path '" (prin1-to-string exec-path) ")\n")
             (write-file (eval-when-compile my-shell_env-file))
             (emacs-lisp-byte-compile))))))

;;; put password in minibuffer
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;;------------------------------------------------------------------------------
;; TRAMP(TransparentRemoteAccessMultipleProtocol)
;; edit remoto file from local emacs
;; emacs default
;;------------------------------------------------------------------------------
(with-eval-after-load 'tramp
  (setq-default tramp-default-method "scp")
  (setq-default tramp-encoding-shell "bash")

  ;; リモートサーバで shell を開いた時に日本語が文字化けしないよう、LC_ALL の設定を無効にする
  ;; http://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-processes.html#Running%20a%20debugger%20on%20a%20remote%20host
  (let ((process-environment (default-value 'tramp-remote-process-environment)))
    (setenv "LC_ALL" nil)
    (setq-default tramp-remote-process-environment process-environment)))

;;------------------------------------------------------------------------------
;; shell-pop
;; シェルバッファをポップアップ
;; from : package system
;;------------------------------------------------------------------------------

;; key-bind
;; (global-set-key [f8] 'shell-pop)
