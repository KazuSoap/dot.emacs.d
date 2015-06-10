;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; SHELL
;;------------------------------------------------------------------------------
;;; shell ----------------------------------------------------------------------
;; shell を有効化
;; emacs default

;; correct exec-path ( 先頭小文字 , 末尾の"/"なし )
(setq exec-path (mapcar #'directory-file-name exec-path))

;; load environment value
(load "shell_env")
(dolist (path (reverse (split-string (getenv "PATH") ";")))
  (add-to-list 'exec-path (directory-file-name path)))

;; load fakecygpty setting
(load "fakecygpty")

(setq explicit-shell-file-name "bash")
(setq shell-command-switch "-c")

;;; (M-! and M-| and compile.el)
(setq shell-file-name "bash")

;; shell の割り込みを機能させる
(defadvice comint-interrupt-subjob (around ad-comint-interrupt-subjob activate)
  (process-send-string nil (kbd "C-c")))
(defadvice comint-stop-subjob (around ad-comint-stop-subjob activate)
  (process-send-string nil (kbd "C-z")))
(defadvice comint-quit-subjob (around ad-comint-quit-subjob activate)
  (process-send-string nil (kbd "C-\\")))
(defadvice comint-send-eof (around ad-comint-send-eof activate)
  (process-send-string nil (kbd "C-d")))

;; shell バッファがカレントの際、動いている process の coding-system 設定を undecided に
(defun set-shell-buffer-process-coding-system ()
  (let ((process (get-buffer-process (buffer-name))))
    (if (and process
             (string-match "^shell" (process-name process)))
        (let ((coding-system (process-coding-system process)))
          (set-process-coding-system process
                                     (coding-system-change-text-conversion
                                      (car coding-system) 'undecided)
                                     (cdr coding-system))))))

;; shellバッファで、コマンド実行結果出力前に set-shell-buffer-process-coding-system を実行する。
;; この設定により、shellバッファで utf-8 の出力をする cygwin コマンドと、cp932 の出力をする
;; Windowsコマンドの漢字の文字化けが回避される。また、漢字を含むプロンプトが文字化けする場合には、
;; .bashrc の PS1 の設定の後に「export PS1="$(sleep 0.1)$PS1"」を追加すれば、回避できる模様。
(defadvice comint-output-filter (before ad-comint-output-filter activate)
  (set-shell-buffer-process-coding-system))

;;; password のミニバッファ入力
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;(setenv "EMACS" "t")
;(shell-command "source ~/.bashrc")
