;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; shell
;; shell を有効化
;; emacs default
;;------------------------------------------------------------------------------

(setq shell-command-switch "-c")

(let ((shell-level (getenv "SHLVL")))
  (when (or (not shell-level) (string= "0" shell-level))
    (when (eq system-type 'windows-nt)
      (setq shell-file-name "d:/msys64/usr/bin/bash")
      (defvar explicit-shell-file-name)
      (setq explicit-shell-file-name shell-file-name)
      (setenv "SHELL" shell-file-name)
      (setenv "MSYSTEM" "MINGW64")

      (defun ad-exec-path-from-shell-setenv (orig-fun &rest args)
        (when (and (string= (car args) "PATH") (fboundp 'cygpath))
          (setf (nth 1 args) (cygpath "-amp" (nth 1 args))))
        (apply orig-fun args))
      (advice-add 'exec-path-from-shell-setenv :around 'ad-exec-path-from-shell-setenv))

    (exec-path-from-shell-copy-envs '("PATH" "MANPATH" "PKG_CONFIG_PATH"))))

(when (eq system-type 'windows-nt)
  ;; shell バッファがカレントの際、動いている process の coding-system 設定を undecided に
  ;; shellバッファで、コマンド実行結果出力前に set-shell-buffer-process-coding-system を実行する。
  ;; この設定により、shellバッファで utf-8 の出力をする cygwin コマンドと、cp932 の出力をする
  ;; Windowsコマンドの漢字の文字化けが回避される。また、漢字を含むプロンプトが文字化けする場合には、
  ;; .bashrc の PS1 の設定の後に「export PS1="$(sleep 0.1)$PS1"」を追加すれば、回避できる模様。
  (defun set-shell-buffer-process-coding-system (&rest args)
    (let ((process (car args)))
      (if (and process (string-match "^shell" (process-name process)))
          (let ((coding-system (process-coding-system process)))
            (set-process-coding-system process
                                       (coding-system-change-text-conversion
                                        (car coding-system) 'undecided)
                                       (cdr coding-system))))))
  (advice-add 'comint-output-filter :before 'set-shell-buffer-process-coding-system)

  ;; emacs-24.4、emacs-24.5 では、4096バイトを超えるデータを一度にパイプ経由で
  ;; プロセスに送り込むと、レスポンスが帰ってこない状況となる。これを改善する。
  ;; (NTEmacsスレッド４ 714 の投稿を一部修正したもの。NTEmacsスレッド４ 734、737 の
  ;; 対策も兼ねるため、 4096バイトを超えない入力の場合でも一律同じ処理を実行している。)
  (defun ad-process-send-string (orig-fun &rest args)
    (if (not (eq (process-type (car args)) 'real))
        (apply orig-fun args)
      (let* ((process (or (car args)
                          (get-buffer-process (current-buffer))))
             (send-string (encode-coding-string (nth 1 args)
                                                (cdr (process-coding-system (get-process process)))))
             (send-string-length (length send-string)))
        (let ((w32-pipe-limit 4096)
              (inhibit-eol-conversion t)
              (from 0)
              to)
          (while (< from send-string-length)
            (setq to (min (+ from w32-pipe-limit) send-string-length))
            (setf (nth 1 args) (substring send-string from to))
            (apply orig-fun args)
            (setq from to))))))
  (advice-add 'process-send-string :around 'ad-process-send-string))

;;; password のミニバッファ入力
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
