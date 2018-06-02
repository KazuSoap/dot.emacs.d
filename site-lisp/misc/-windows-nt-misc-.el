;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; fix mount point for msys2
;;------------------------------------------------------------------------------
(eval-when-compile
  (defconst msys-root
    (ignore-errors
      (expand-file-name
       (nth 3 (split-string
               (shell-command-to-string "reg query \"HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\" -v InstallLocation -s | findstr \"msys64\"") " +\\|\n")))))

  (defmacro apply-fixed-mountpoint-internal (nth_arg)
       `(defsubst ,(intern (format "apply-fixed-mountpoint-internal_%s" nth_arg)) (args)
               (and (string-match "^/" (nth ,nth_arg args))
                    (cond ((string-match "^/\\([A-Za-z]\\)\\(/\\|$\\)" (nth ,nth_arg args))
                           (setf (nth ,nth_arg args) (replace-match "\\1:\\2" nil nil (nth ,nth_arg args))))
                          ((string-match "^/home\\(/\\|$\\)" (nth ,nth_arg args))
                           (setf (nth ,nth_arg args) ,(file-name-directory (getenv "HOME"))))
                          ((string-match "^/bin\\(/\\|$\\)" (nth ,nth_arg args))
                           (setf (nth ,nth_arg args) (concat ,(concat msys-root "/usr") (nth ,nth_arg args))))
                          (t ;; else
                           (setf (nth ,nth_arg args) (concat ,msys-root (nth ,nth_arg args))))))
               args))
  (apply-fixed-mountpoint-internal 0)) ;; apply-fixed-mountpoint-internal_0

(fset 'apply-fixed-mountpoint_0
      (lambda (args) (apply-fixed-mountpoint-internal_0 args)))

(advice-add 'substitute-in-file-name :filter-args 'apply-fixed-mountpoint_0)
(advice-add 'expand-file-name :filter-args 'apply-fixed-mountpoint_0)
(advice-add 'locate-file-internal :filter-args 'apply-fixed-mountpoint_0)
(advice-add 'helm-ff-set-pattern :filter-args 'apply-fixed-mountpoint_0)

;;------------------------------------------------------------------------------
;; cygpath
;;------------------------------------------------------------------------------
(fset 'cygpath
      (lambda (&optional option path)
        "cygpath for emacs lisp"
        (if path
            (with-temp-buffer
              (call-process (eval-when-compile (expand-file-name "/bin/cygpath")) nil '(t nil) nil option path)
              (unless (bobp)
                (goto-char (point-min))
                (buffer-substring-no-properties (point) (line-end-position)))))))

;;------------------------------------------------------------------------------
;; IME
;;------------------------------------------------------------------------------
;; IMEのカスタマイズ
(setq-default default-input-method "W32-IME") ;;標準IMEの設定

;; Windows IME の ON:[あ]/OFF:[Aa] をモードラインに表示
(setq-default w32-ime-mode-line-state-indicator "[Aa]")
(setq-default w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

;; IME の初期化
(w32-ime-initialize)

;; IME ON/OFF時のカーソルカラー
(add-hook 'w32-ime-on-hook (lambda () (set-cursor-color "yellow")))
(add-hook 'w32-ime-off-hook (lambda () (set-cursor-color "thistle")))

;; IMEの制御(yes/noをタイプするところでは IME をオフにする)
(wrap-function-to-control-ime #'universal-argument t nil)
(wrap-function-to-control-ime #'read-string nil nil)
(wrap-function-to-control-ime #'read-char nil nil)
(wrap-function-to-control-ime #'read-from-minibuffer nil nil)
(wrap-function-to-control-ime #'y-or-n-p nil nil)
(wrap-function-to-control-ime #'yes-or-no-p nil nil)
(wrap-function-to-control-ime #'map-y-or-n-p nil nil)

;; fix w32 ime bug
(menu-bar-open)
(fset 'send-esc
      (lambda ()
        (start-process "my-proc" nil "cscript.exe"
                       (eval-when-compile (expand-file-name "sendesc.js" user-emacs-directory)))))
(add-hook 'emacs-startup-hook 'send-esc)

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
(fset 'set-shell-buffer-process-coding-system
      (lambda (&rest args)
        (let ((process (car args)))
          (if (and process (string-match "^shell\\|^terminal" (process-name process)))
              ;; process
              (let ((coding-system (process-coding-system process)))
                (set-process-coding-system process
                                           (coding-system-change-text-conversion
                                            (car coding-system) 'undecided)
                                           (cdr coding-system)))))))
(advice-add 'comint-output-filter :before 'set-shell-buffer-process-coding-system)
(advice-add 'term-emulate-terminal :before 'set-shell-buffer-process-coding-system)

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
;; exec-path-from-shell
;; shell から PATH の設定を引き継ぐ
;; from package
;;------------------------------------------------------------------------------
(with-eval-after-load 'exec-path-from-shell
  (setenv "SHELL" (eval-when-compile (expand-file-name "/bin/bash")))
  (setq-default explicit-shell-file-name (setq shell-file-name (getenv "SHELL")))

  (fset 'ad-exec-path-from-shell-setenv
        (lambda (args)
          (when (and (string= (car args) "PATH") (fboundp 'cygpath))
            (setf (nth 1 args) (cygpath "-amp" (nth 1 args))))
          args))
  (advice-add 'exec-path-from-shell-setenv :filter-args 'ad-exec-path-from-shell-setenv))

;;------------------------------------------------------------------------------
;; magit
;; git クライアント
;; from package
;;------------------------------------------------------------------------------
;; (with-eval-after-load 'magit
;;   (fset 'ad-magit-process-file
;;         (lambda (args)
;;           (mapcar (lambda (x)
;;                     (if (and (stringp x) (string-match-p "\\^{.*}" x))
;;                         (shell-quote-argument x)
;;                       x))
;;                   args)))
;;   (advice-add 'magit-process-file :filter-args 'ad-magit-process-file))

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
  (defvar ex-irony--install-server-read-cmd "\\1 -G'MSYS Makefiles' -DLIBCLANG_LIBRARY=/mingw64/bin/clang.dll"))
