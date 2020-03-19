;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; IME
;;------------------------------------------------------------------------------
(autoload 'w32-imm32-on-start-enabler-inject "w32-imm32-on-start-enabler" t nil)
(global-set-key (kbd "<non-convert>")
                (lambda ()
                  (interactive)
                  (global-unset-key (kbd "<non-convert>"))
                  (when (load "lisp-w32-imeadv" nil t)
                    (w32-imm32-on-start-enabler-inject)
                    (setq-default w32-imeadv-ime-status-line-indicate-close "[Aa]")
                    (setq-default w32-imeadv-status-line "[Aa]")
                    (setq-default w32-imeadv-ime-openstatus-indicate-cursor-color-enable t)
                    (setq-default w32-imeadv-ime-openstatus-indicate-cursor-color "yellow")
                    (setq-default w32-imeadv-ime-closestatus-indicate-cursor-color "thistle")
                    (force-mode-line-update t))))

;;------------------------------------------------------------------------------
;; 印刷設定
;;------------------------------------------------------------------------------
;; lpr-bufferコマンド で notepad を開くようにする
(with-eval-after-load 'lpr
  (setq-default print-region-function
                (lambda (start end _program &optional _delete _destination _display &rest _args)
                  (let* ((procname (make-temp-name "w32-print-"))
                         (winfile (expand-file-name procname temporary-file-directory)))
                    (write-region start end winfile)
                    (set-process-sentinel
                     (start-process procname nil "notepad.exe" winfile)
                     (lambda (_process _state)
                       (when (file-exists-p winfile)
                         (delete-file winfile))))))))

;;------------------------------------------------------------------------------
;; fakecygpty
;; NTEmacs の仮想端末偽装
;; https://github.com/d5884/fakecygpty
;;------------------------------------------------------------------------------
(autoload 'fakecygpty-activate "fakecygpty" t nil)
(add-hook 'after-init-hook #'fakecygpty-activate)

;;------------------------------------------------------------------------------
;; cygwin-mount
;; Teach EMACS about cygwin styles and mount points.
;; https://www.emacswiki.org/emacs/cygwin-mount.el
;;------------------------------------------------------------------------------
(autoload 'cygwin-mount-activate "cygwin-mount" t nil)
(add-hook 'after-init-hook #'cygwin-mount-activate)

(eval-when-compile
  (require 'cygwin-mount)

  (defmacro my-cygwin-mount-config ()
    (when (string-match ".elc$" (or (locate-library "-windows-nt-misc-") ""))
      (cygwin-mount-build-table-internal)
      `(progn
         (setq-default cygwin-mount-table ',cygwin-mount-table--internal)
         (advice-add 'cygwin-mount-get-cygdrive-prefix :override (lambda () ,(cygwin-mount-get-cygdrive-prefix)))))))

(with-eval-after-load 'cygwin-mount
  (my-cygwin-mount-config))


;;------------------------------------------------------------------------------
;; exec-path-from-shell
;; shell から PATH の設定を引き継ぐ
;; from package
;;------------------------------------------------------------------------------
(with-eval-after-load 'exec-path-from-shell
  (declare-function cygpath "site-start")
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
  ;; Windows Subsystem for Linux(WSL) の irony-server と共存するための設定
  (setq-default irony-server-install-prefix (concat (default-value 'irony-server-install-prefix) "win-nt/"))

  ;; Windows performance tweaks
  (setq-default w32-pipe-read-delay 0)

  ;; Set the buffer size to 64K on Windows (from the original 4K)
  (setq-default irony-server-w32-pipe-buffer-size (* 64 1024))

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
