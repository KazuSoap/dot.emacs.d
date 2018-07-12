;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; shell
;;------------------------------------------------------------------------------
(eval-when-compile
  (defconst my-env-var-list '("SHELL" "PATH" "MANPATH" "PKG_CONFIG_PATH" "LANG" "JAVA_HOME" "GRAPHVIZ_DOT"))

  (defmacro setenv_cached-env-var (env-var-lst)
    (cons 'progn
          (mapcar (lambda (x)
                    (if (string-match "SHELL" x)
                        `(setq-default shell-file-name (setenv ,x ,(getenv x)))
                      `(setenv ,x ,(getenv x))))
                  (eval env-var-lst)))))

(when (string= "0" (getenv "SHLVL"))
  (cond ((time-less-p (eval-when-compile (nth 5 (file-attributes (file-truename "~/.bash_profile"))))
                      (nth 5 (file-attributes (eval-when-compile (file-truename "~/.bash_profile")))))
         ;; sync emacs environment variable with shell's one
         (exec-path-from-shell-copy-envs (eval-when-compile my-env-var-list))
         (add-hook 'after-init-hook (lambda () (byte-compile-file (locate-library "-shell-.el")))))
        (t ;; else
         ;; setenv cached environment variables
         (setenv_cached-env-var (eval-when-compile my-env-var-list))
         (setq exec-path (eval-when-compile exec-path)))))

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
