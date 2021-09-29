;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; magit
;; git クライアント
;; from package
;;------------------------------------------------------------------------------
;; (with-eval-after-load 'magit-utils
;;   (unless (get-process "my-bash-process")
;;     (start-process "my-bash-process" "my-bash" "bash")
;;     (set-process-query-on-exit-flag (get-process "my-bash-process") nil)))

;;------------------------------------------------------------------------------
;; irony
;; A C/C++ minor mode powered by libclang
;; from package
;;------------------------------------------------------------------------------
(with-eval-after-load 'irony
  (eval-when-compile (require 'irony))

  ;; Windows Subsystem for Linux(WSL) の irony-server と共存するための設定
  ;; (setq-default irony-server-install-prefix (concat (default-value 'irony-server-install-prefix) "win-nt/"))

  ;; Set the buffer size to 64K on Windows (from the original 4K)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024))

  ;; irony-server-install に失敗する問題の修正
  ;; コンパイラに clang を指定
  (fset 'ad-irony--install-server-read-command
        (lambda (args)
          "modify irony--install-server-read-command"
          ;; (setenv "CC" "clang") (setenv "CXX" "clang++")
          `(,(replace-regexp-in-string
              (format "^\\(%s\\)" (shell-quote-argument (default-value 'irony-cmake-executable)))
              "\\1 -G'MSYS Makefiles'"
              (car args)))))
  (advice-add 'irony--install-server-read-command :filter-args 'ad-irony--install-server-read-command)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))
