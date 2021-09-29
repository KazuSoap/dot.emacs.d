;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; cygwin-mount
;; Teach EMACS about cygwin styles and mount points.
;; https://www.emacswiki.org/emacs/cygwin-mount.el
;;------------------------------------------------------------------------------
(autoload 'cygwin-mount-activate "cygwin-mount" t nil)
(add-hook 'after-init-hook #'cygwin-mount-activate)

(eval-when-compile
  (require 'cygwin-mount)
  (defvar msys-bin (file-name-directory shell-file-name))
  (setq cygwin-mount-program (concat msys-bin "mount.exe"))
  (setq cygwin-mount-uname-program (concat msys-bin "uname.exe"))

  (defmacro my-cygwin-mount-config ()
    (cygwin-mount-build-table-internal)
    `(progn
       (setq cygwin-mount-table ',cygwin-mount-table--internal)
       ;; (advice-add 'cygwin-mount-get-cygdrive-prefix :override (lambda () ,(cygwin-mount-get-cygdrive-prefix)))
       (fset 'cygwin-mount-get-cygdrive-prefix (lambda () ,(cygwin-mount-get-cygdrive-prefix))))))

(with-eval-after-load 'cygwin-mount
  (my-cygwin-mount-config))

;;------------------------------------------------------------------------------
;; exec-path-from-shell
;; shell から PATH の設定を引き継ぐ
;; from package
;;------------------------------------------------------------------------------
(with-eval-after-load 'exec-path-from-shell
  (eval-when-compile (declare-function cygpath "early-init"))
  (fset 'ad-exec-path-from-shell-setenv
        (lambda (args)
          (and (string= (car args) "PATH")
               (setf (nth 1 args) (cygpath "-amp" (nth 1 args))))
          args))
  (advice-add 'exec-path-from-shell-setenv :filter-args 'ad-exec-path-from-shell-setenv))

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
