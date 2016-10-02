;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; win-misc
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; local functions & macro
;;------------------------------------------------------------------------------
(defun cygpath (&optional option path)
  "cygpath for emacs lisp"
  (if path (with-temp-buffer
             (call-process "d:/msys64/usr/bin/cygpath" nil '(t nil) nil option path)
             (unless (bobp)
               (goto-char (point-min))
               (buffer-substring-no-properties (point) (line-end-position))))))

;;------------------------------------------------------------------------------
;; process I/O
;;------------------------------------------------------------------------------
;; サブプロセスが出力する文字コードを判定して、process-coding-system の設定値を決定
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; サブプロセスに渡すパラメータの文字コードを cp932 にする
(defmacro set-function-args-encode (fun-name args-number)
  `(progn
     (defun  ,(intern (format "ad-%s" fun-name)) (orig-fun &rest args)
       (if (nthcdr ,args-number args)
           (setf (nthcdr ,args-number args)
                 (mapcar (lambda (arg)
                           (if (multibyte-string-p arg)
                               (encode-coding-string arg 'cp932)
                             arg))
                         (nthcdr ,args-number args))))
       (apply orig-fun args))
     (advice-add (quote ,fun-name) :around (quote ,(intern (format "ad-%s" fun-name))))))

(set-function-args-encode call-process-region 6)
(set-function-args-encode call-process 4)
(set-function-args-encode start-process 3)

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
;; IME
;;------------------------------------------------------------------------------
;; ;;-- IME customize --;;
;; ;; IME ON/OFF 時のカーソルカラー設定用関数
;; (defun w32-ime-on-hooks () (set-cursor-color "yellow"))
;; (defun w32-ime-off-hooks () (set-cursor-color "thistle"))

;; ;; IMEのカスタマイズ
;; (setq default-input-method "W32-IME") ;;標準IMEの設定

;; ;; Windows IME の ON:[あ]/OFF:[Aa] をモードラインに表示
;; (setq-default w32-ime-mode-line-state-indicator "[Aa]")
;; (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

;; ;; IME の初期化
;; (w32-ime-initialize)

;; ;; IME ON/OFF時のカーソルカラー
;; (add-hook 'w32-ime-on-hook 'w32-ime-on-hooks)
;; (add-hook 'w32-ime-off-hook 'w32-ime-off-hooks)

;; ;; IMEの制御(yes/noをタイプするところでは IME をオフにする)
;; (wrap-function-to-control-ime 'universal-argument t nil)
;; (wrap-function-to-control-ime 'read-string nil nil)
;; (wrap-function-to-control-ime 'read-char nil nil)
;; (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
;; (wrap-function-to-control-ime 'y-or-n-p nil nil)
;; (wrap-function-to-control-ime 'yes-or-no-p nil nil)
;; (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
