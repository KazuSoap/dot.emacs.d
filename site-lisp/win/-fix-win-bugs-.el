;;------------------------------------------------------------------------------
;; process I/O
;;------------------------------------------------------------------------------
;; サブプロセスが出力する文字コードを判定して、process-coding-system の設定値を決定
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; サブプロセスに渡すパラメータの文字コードを cp932 にする
(defmacro set-function-args-encode (fun-name)
  `(progn
     (defun ,(intern (format "ad-%s" fun-name)) (args)
       (mapcar ,(lambda (arg)
                  (if (multibyte-string-p arg)
                      (encode-coding-string arg 'cp932)
                    arg))
               args))
     (advice-add ',fun-name :filter-args ',(intern (format "ad-%s" fun-name)))))

(set-function-args-encode call-process-region)
(set-function-args-encode call-process)
(set-function-args-encode start-process)

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

;; emacs-24.4 or later において、4096 byte を超えるデータを一度に pipe 経由で
;; サブプロセスに送り込むと、レスポンスが帰ってこなくなる。
;; -> 4096 byte ごとにデータを区切って pipe に送るようにする。
;; 参考：2ch NTEmacsスレッド４ 699 以降
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
(advice-add 'process-send-string :around 'ad-process-send-string)

;;------------------------------------------------------------------------------
;; exec-path-from-shell
;; shell から PATH の設定を引き継ぐ
;; from package
;;------------------------------------------------------------------------------
(let ((shell-level (getenv "SHLVL")))
  (when (or (not shell-level) (string= "0" shell-level))
    (setq shell-file-name "d:/msys64/usr/bin/bash")
    (defvar explicit-shell-file-name)
    (setq explicit-shell-file-name shell-file-name)
    (setenv "SHELL" shell-file-name)
    (setenv "MSYSTEM" "MINGW64")

    (defun ad-exec-path-from-shell-setenv (args)
      (when (and (string= (car args) "PATH") (fboundp 'cygpath))
        (setf (nth 1 args) (cygpath "-amp" (nth 1 args))))
      args)
    (advice-add 'exec-path-from-shell-setenv :filter-args 'ad-exec-path-from-shell-setenv)))

;;------------------------------------------------------------------------------
;; fakecygpty
;; NTEmacs の仮想端末偽装
;; https://github.com/d5884/fakecygpty
;;------------------------------------------------------------------------------
(defun ad-exec-path-from-shell-copy-envs (&rest args)
  (require 'fakecygpty)
  (fakecygpty-activate))

(let ((shell-level (getenv "SHLVL")))
  (unless (or (not shell-level) (string= "0" shell-level))
    (ad-exec-path-from-shell-copy-envs)))

(advice-add 'exec-path-from-shell-copy-envs :after 'ad-exec-path-from-shell-copy-envs)

;;------------------------------------------------------------------------------
;; ggtags
;; タグジャンプツール. GNU Global を利用
;; from : package system
;;------------------------------------------------------------------------------

(defun ad-ggtags-ensure-localname (&rest args)
  "convert Windows path to UNIX path"
  (when (fboundp 'cygpath) (cygpath "-u" (car args))))
(advice-add 'ggtags-ensure-localname :filter-return 'ad-ggtags-ensure-localname)

(defun ad-ggtags-process-string (orig-func &rest args)
  "if execute global -pr command, convert UNIX path to Windows path"
  (if (and (string-match "global -p" (format "%s" args))
           (fboundp 'cygpath))
      (cygpath "-am" (apply orig-func args))
    (apply orig-func args)))
(advice-add 'ggtags-process-string :around 'ad-ggtags-process-string)

;;------------------------------------------------------------------------------
;; magit
;; git クライアント
;; from package
;;------------------------------------------------------------------------------

(defun ad-magit-read-repository (args)
  (when (file-directory-p args) (file-truename args)))
(advice-add 'magit-read-repository :filter-return 'ad-magit-read-repository)

;;------------------------------------------------------------------------------
;; irony
;; A C/C++ minor mode powered by libclang
;; from package
;;------------------------------------------------------------------------------

(with-eval-after-load 'irony
  ;; bash on Ubuntu on Windows の irony-server と共存するための設定
  (defvar irony-server-install-prefix)
  (setq irony-server-install-prefix (concat irony-server-install-prefix "win-nt/"))

  ;; Windows performance tweaks
  (setq w32-pipe-read-delay 0)

  (defvar irony-server-w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
