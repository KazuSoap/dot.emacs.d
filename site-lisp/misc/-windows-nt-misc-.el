;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; IME
;;------------------------------------------------------------------------------
(eval-when-compile
  (defvar w32-ime-mode-line-state-indicator)
  (defvar w32-ime-mode-line-state-indicator-list)
  (declare-function tr-ime-hook-check "tr-ime-hook")
  (declare-function w32-ime-wrap-function-to-control-ime "w32-ime")
  (declare-function tr-ime-font-reflect-frame-parameter "tr-ime-font"))

;; 無変換キーで tr-ime & w32-ime を有効化
(global-set-key (kbd "<non-convert>")
                (lambda ()
                  (interactive)
                  (global-unset-key (kbd "<non-convert>"))

                  ;; tr-imeの有効化
                  ;; (tr-ime-advanced-install)
                  (tr-ime-advanced-initialize)
                  (tr-ime-hook-check)))

(with-eval-after-load 'w32-ime
  ;; 標準IMEの設定
  (setq default-input-method "W32-IME")

  ;; Windows IME の ON:[あ]/OFF:[Aa] をモードラインに表示
  (setq w32-ime-mode-line-state-indicator "[Aa]")
  (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

  ;; IME の初期化
  (w32-ime-initialize)

  ;; IME ON/OFF時のカーソルカラー
  (add-hook 'w32-ime-on-hook (lambda () (set-cursor-color "yellow")))
  (add-hook 'w32-ime-off-hook (lambda () (set-cursor-color "thistle")))

  ;; IMEの制御(yes/noをタイプするところでは IME をオフにする)
  (w32-ime-wrap-function-to-control-ime #'universal-argument)
  (w32-ime-wrap-function-to-control-ime #'read-string)
  (w32-ime-wrap-function-to-control-ime #'read-char)
  (w32-ime-wrap-function-to-control-ime #'read-from-minibuffer)
  (w32-ime-wrap-function-to-control-ime #'y-or-n-p)
  (w32-ime-wrap-function-to-control-ime #'yes-or-no-p)
  (w32-ime-wrap-function-to-control-ime #'map-y-or-n-p)

  ;; frame font
  (modify-all-frames-parameters `((ime-font . ,(frame-parameter nil 'font))))
  (tr-ime-font-reflect-frame-parameter))

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
