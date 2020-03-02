;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; fix mount point for msys2
;;------------------------------------------------------------------------------
;;   ;; (defmacro apply-fixed-mountpoint (nth_arg)
;;   ;;   `(lambda (args)
;;   ;;      (and (string-match "^/" (nth ,nth_arg args))
;;   ;;           (cond ((string-match "^/\\([A-Za-z]\\)\\(/\\|$\\)" (nth ,nth_arg args))
;;   ;;                  (setf (nth ,nth_arg args) (replace-match "\\1:\\2" nil nil (nth ,nth_arg args))))
;;   ;;                 ((string-match "^/home\\(/\\|$\\)" (nth ,nth_arg args))
;;   ;;                  (setf (nth ,nth_arg args) (replace-match ,(file-name-directory (getenv "HOME")) nil nil (nth ,nth_arg args))))
;;   ;;                 ((string-match "^/bin\\(/\\|$\\)" (nth ,nth_arg args))
;;   ;;                  (setf (nth ,nth_arg args) (concat ,(concat msys-root "/usr") (nth ,nth_arg args))))
;;   ;;                 (t ;; else
;;   ;;                  (setf (nth ,nth_arg args) (concat ,msys-root (nth ,nth_arg args))))))
;;   ;;      args))
;;   )

;; ;; (fset 'apply-fixed-mountpoint_0 (apply-fixed-mountpoint 0))

;; ;; (advice-add 'substitute-in-file-name :filter-args 'apply-fixed-mountpoint_0)
;; ;; (advice-add 'expand-file-name :filter-args 'apply-fixed-mountpoint_0)
;; ;; (advice-add 'locate-file-internal :filter-args 'apply-fixed-mountpoint_0)
;; ;; (advice-add 'helm-ff-set-pattern :filter-args 'apply-fixed-mountpoint_0)

;;------------------------------------------------------------------------------
;; cygpath
;;------------------------------------------------------------------------------
(fset 'cygpath
      (lambda (&optional option path)
        "cygpath for emacs lisp"
        (if path
            (with-temp-buffer
              ;; (call-process (eval-when-compile (concat (get-msys-root) "/usr/bin/cygpath")) nil '(t nil) nil option path)
              (call-process (concat (get-msys-root) "/usr/bin/cygpath") nil '(t nil) nil option path)
              (unless (bobp)
                (goto-char (point-min))
                (buffer-substring-no-properties (point) (line-end-position)))))))

;;------------------------------------------------------------------------------
;; IME
;;------------------------------------------------------------------------------
(declare-function w32-imm32-on-start-enabler-inject "w32-imm32-on-start-enabler")
(global-set-key (kbd "<non-convert>")
                (lambda ()
                  (interactive)
                  (global-unset-key (kbd "<non-convert>"))
                  (when (and (locate-library "w32-imeadv")
                             (locate-library "w32-imm32-on-start-enabler-impl")
                             (load "lisp-w32-imeadv" nil t)
                             (require 'w32-imm32-on-start-enabler))
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
;; 文字化け防止
(setq-default ps-multibyte-buffer 'non-latin-printer)

;; lpr-bufferコマンド で notepad を開くようにする
(setq-default print-region-function
              (lambda (start end _program &optional _delete _destination _display &rest _args)
                (let* ((procname (make-temp-name "w32-print-"))
                       (winfile (expand-file-name procname temporary-file-directory)))
                  (let ((coding-system-for-write 'cp932-dos))
                    (write-region start end winfile))
                  (set-process-sentinel
                   (start-process procname nil "notepad.exe" winfile)
                   (lambda (_process _state)
                     (when (file-exists-p winfile)
                       (delete-file winfile)))))))
;; lpr-buffer を実行する
(global-set-key (kbd "C-c C-p") #'lpr-buffer)

;;------------------------------------------------------------------------------
;; process I/O
;;------------------------------------------------------------------------------
;; サブプロセスに渡すパラメータの文字コードを cp932 にする
(fset 'set-function-args-encode
      (lambda (args)
        (mapcar (lambda (arg)
                  (if (multibyte-string-p arg)
                      (encode-coding-string arg 'cp932)
                    arg))
                args)))

(advice-add 'call-process-region :filter-args 'set-function-args-encode)
(advice-add 'call-process :filter-args 'set-function-args-encode)
(advice-add 'start-process :filter-args 'set-function-args-encode)

;; shell バッファがカレントの際、動いている process の coding-system 設定を undecided に
;; shellバッファで、コマンド実行結果出力前に set-shell-buffer-process-coding-system を実行する。
;; この設定により、shellバッファで utf-8 の出力をする cygwin コマンドと、cp932 の出力をする
;; Windowsコマンドの漢字の文字化けが回避される。また、漢字を含むプロンプトが文字化けする場合には、
;; .bashrc の PS1 の設定の後に「export PS1="$(sleep 0.1)$PS1"」を追加すれば、回避できる模様。
(eval-when-compile
  (defmacro set-process-coding-system-undecided (&optional is_term)
    (let ((shell-cond
           (if is_term 'process
             '(and process (string-match "^shell" (process-name process))))))
      `(lambda (&rest args)
         (let ((process (car args)))
           (if ,shell-cond
               (let ((coding-system (process-coding-system process)))
                 (set-process-coding-system process
                                            (coding-system-change-text-conversion
                                             (car coding-system) 'undecided)
                                            (cdr coding-system)))))))))

(fset 'set-shell-process-coding-system (set-process-coding-system-undecided))
(fset 'set-term-process-coding-system (set-process-coding-system-undecided t))

(advice-add 'comint-output-filter :before 'set-shell-process-coding-system)
(advice-add 'term-emulate-terminal :before 'set-term-process-coding-system)

;;------------------------------------------------------------------------------
;; shell
;; shell を有効化
;; emacs default
;;------------------------------------------------------------------------------
(setenv "MSYSTEM" "MINGW64")
(or (getenv "SHLVL") (setenv "SHLVL" "0"))

;;------------------------------------------------------------------------------
;; fakecygpty
;; NTEmacs の仮想端末偽装
;; https://github.com/d5884/fakecygpty
;;------------------------------------------------------------------------------
(autoload 'fakecygpty-activate "fakecygpty" t nil)
(add-hook 'after-init-hook #'fakecygpty-activate) ;; fakecygpty の有効化

;;------------------------------------------------------------------------------
;; cygwin-mount
;; Teach EMACS about cygwin styles and mount points.
;; https://www.emacswiki.org/emacs/cygwin-mount.el
;;------------------------------------------------------------------------------
(require 'cygwin-mount)
(add-hook 'after-init-hook #'cygwin-mount-activate)


;;------------------------------------------------------------------------------
;; exec-path-from-shell
;; shell から PATH の設定を引き継ぐ
;; from package
;;------------------------------------------------------------------------------
(with-eval-after-load 'exec-path-from-shell
  (setenv "SHELL" (eval-when-compile (concat (get-msys-root) "/usr/bin/bash")))
  (setq-default explicit-shell-file-name (setq shell-file-name (getenv "SHELL")))

  (fset 'ad-exec-path-from-shell-setenv
        (lambda (args)
          (and (string= (car args) "PATH") (fboundp 'cygpath)
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

  ;; irony-server-install に失敗する問題の修正用
  (defvar ex-irony--install-server-read-cmd "\\1 -G'MSYS Makefiles'"))
