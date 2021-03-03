;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; windows-misc
;;------------------------------------------------------------------------------
(eval-when-compile
  (defconst msys-root
    (let* ((reg_hkcu_uninstall_key "\"HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\"")
           (reg_query_cmd (concat "reg query " reg_hkcu_uninstall_key " -v InstallLocation -s | findstr msys64")))
      (ignore-errors
        (expand-file-name
         (nth 3 (split-string (shell-command-to-string reg_query_cmd) " +\\|\n")))))
    "Get the installation location of \"msys2\"")

  (defmacro windows-nt-core ()
    "Windows Specific Settings"
    (when (eq system-type 'windows-nt)
      `(progn
         ;; Set environment variable
         (setenv "HOME" (concat ,(concat msys-root "/home/") user-login-name))
         (setenv "SHELL" ,(concat msys-root "/usr/bin/bash"))
         (setenv "MSYSTEM" "MINGW64")
         (or (getenv "SHLVL") (setenv "SHLVL" "0"))
         (setq-default shell-file-name ,(getenv "SHELL"))

         ;; coding-system
         ;; デフォルトの文字コードを設定
         ;; 指定される文字コードは以下の項目
         ;; ① ファイルを新規作成した場合のデフォルト
         ;; ② サブプロセスでの IO
         ;; ③ 他の項目が指定されていない場合のデフォルト値
         (prefer-coding-system 'utf-8-unix)

         (fset 'cygpath
               (lambda (&optional option path)
                 "cygpath for emacs lisp"
                 (when path
                   (with-temp-buffer
                     (call-process ,(concat msys-root "/usr/bin/cygpath") nil '(t nil) nil option path)
                     (unless (bobp)
                       (goto-char (point-min))
                       (buffer-substring-no-properties (point) (line-end-position)))))))

         (fset 'set-function-args-encode
               (lambda (args)
                 "Set the character code of the parameter passed to the subprocess to cp932"
                 (mapcar (lambda (arg)
                           (if (multibyte-string-p arg)
                               (encode-coding-string arg 'cp932)
                             arg))
                         args)))

         (advice-add 'call-process-region :filter-args 'set-function-args-encode)
         (advice-add 'call-process :filter-args 'set-function-args-encode)
         (advice-add 'start-process :filter-args 'set-function-args-encode)))))
(windows-nt-core)

;;------------------------------------------------------------------------------
;; garbage collection
;;------------------------------------------------------------------------------
(setq-default gc-cons-threshold (* 128 1024 1024))

;;------------------------------------------------------------------------------
;; face & frame parameters
;;------------------------------------------------------------------------------
;; color-theme
(load-theme 'wheatgrass t)
(set-face-attribute 'mode-line nil :foreground "gray85" :background "#4a5459")
(set-face-attribute 'fringe nil :background "black")

;; frame parameters
(setq default-frame-alist
      '((width . 100)
        (height . 30)
        (alpha . 85)
        (font . "fontset-myricty")
        ))

;;------------------------------------------------------------------------------
;; global minor-mode
;;------------------------------------------------------------------------------
(tool-bar-mode -1) ; tool-bar-mode
;; (menu-bar-mode -1) ; menu-bar-mode (default)
(fringe-mode -1) ; fringe-mode
(column-number-mode 1) ; モードラインに列番号表示
;; (line-number-mode -1) ; モードラインに行番号表示 (default)
;; (size-indication-mode -1) ; モードラインにファイルサイズ表示 (default)
(delete-selection-mode 1) ; リージョン上書き

;;------------------------------------------------------------------------------
;; common-misc
;;------------------------------------------------------------------------------
;; "yes or no"を"y or n"に
(fset 'yes-or-no-p #'y-or-n-p)

;; C-hでBS, shift+C-hでHelp
(keyboard-translate ?\C-h ?\C-?) ; translate `C-h' to BS
(keyboard-translate ?\C-? ?\C-h)  ; translate BS to `C-h'

;; ファイルのフルパスをタイトルバーに表示
(setq-default frame-title-format '((:eval (if (buffer-file-name) "%f" "%b")) " - Emacs"))

;; beep音 off
(setq-default ring-bell-function #'ignore)

;; don't make BackUp file
(setq-default auto-save-default nil) ;; #*
(setq-default make-backup-files nil) ;; *.~

;; インデントは tab でなく 半角スペース
(setq-default indent-tabs-mode nil)

;; 双方向テキスト
(setq-default bidi-display-reordering nil) ;; 双方向テキスト可否
;; (setq-default bidi-paragraph-direction 'left-to-right) ;; テキスト方向を強制 (default)

;; mouse scroll
(setq-default mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control))))

;; デフォルトのメジャーモード
(setq-default major-mode #'text-mode)

;; *scratch* バッファのメジャーモード
;; (setq-default initial-major-mode 'fundamental-mode)

;;------------------------------------------------------------------------------
;; local functions
;;------------------------------------------------------------------------------
;;; my-message-startup-time ----------------------------------------------------
;; 起動時間を [ms] 単位で表示
;; emacs-init-time() は 秒単位で小数点第一位まで
;; http://memo.sugyan.com/entry/20120120/1327037494
(fset 'my-message-startup-time
      (lambda ()
        "echo bootup time in message buffer"
        (message "Emacs loaded in %d ms"
                 (* 1000 (float-time (time-subtract after-init-time before-init-time))))))
(add-hook 'after-init-hook 'my-message-startup-time)

;;; my-revert-buffer-no-confirm ------------------------------------------------
;; バッファを再読み込み
;; https://www.emacswiki.org/emacs/RevertBuffer
(fset 'my-revert-buffer-no-confirm
      (lambda (&optional force-reverting)
        (interactive "P")
        (if (or force-reverting (not (buffer-modified-p)))
            (revert-buffer :ignore-auto :noconfirm)
          (error "The buffer has been modified"))))
(global-set-key (kbd "<f5>") 'my-revert-buffer-no-confirm)

;;; my-window-resizer ----------------------------------------------------------
;; window size を調整
;; http://d.hatena.ne.jp/khiker/20100119/window_resize
;; (fset 'my-window-resizer
;;       (lambda ()
;;         "Control window size and position."
;;         (interactive)
;;         (let ((dx (if (= (nth 0 (window-edges)) 0) 1 -1))
;;               (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
;;               action
;;               c)
;;           (catch 'end-flag
;;             (while t
;;               (setq action (read-key-sequence-vector (format "size[%dx%d]" (window-width) (window-height))))
;;               (setq c (aref action 0))
;;               (cond ((= c ?l) (enlarge-window dx t))
;;                     ((= c ?h) (shrink-window dx t))
;;                     ((= c ?j) (enlarge-window dy))
;;                     ((= c ?k) (shrink-window dy))
;;                     (t (let ((command (key-binding action)))
;;                          (when command (call-interactively command)))
;;                        (message "Quit")
;;                        (throw 'end-flag t))))))))
;; (global-set-key (kbd "C-c C-r") 'my-window-resizer)

;;; my-mark-eob ----------------------------------------------------------------
;; バッファの最後に "[EOB]" を表示
;; http://www.emacswiki.org/cgi-bin/wiki?HighlightEndOfBuffer
(fset 'my-mark-eob
      (lambda ()
        (let ((existing-overlays (overlays-in (point-max) (point-max)))
              (eob-mark (make-overlay (point-max) (point-max) nil t t))
              (eob-text "[EOB]"))
          ;; Delete any previous EOB markers.  Necessary so that they don't
          ;; accumulate on calls to revert-buffer.
          (dolist (next-overlay existing-overlays)
            (if (overlay-get next-overlay 'eob-overlay)
                (delete-overlay next-overlay)))
          ;; Add a new EOB marker.
          (put-text-property 0 (length eob-text)
                             'face '(foreground-color . "slate gray") eob-text)
          (overlay-put eob-mark 'eob-overlay t)
          (overlay-put eob-mark 'after-string eob-text))))
(add-hook 'find-file-hook 'my-mark-eob)

;;------------------------------------------------------------------------------
;; local macros
;;------------------------------------------------------------------------------
;;; my-debug-message -----------------------------------------------------------
;; 特定の関数を実行された際、引数と出力、backtrace を出力
;; (eval-when-compile
;;   (defmacro my-debug-message (fun-name)
;;     `(progn
;;        (fset (quote ,(intern (format "add-%s" fun-name)))
;;              (lambda (f &rest args)
;;                (backtrace)
;;                ;; (message "(%s)before:%s" ,(format "%s" fun-name) (prin1-to-string args))
;;                (message "(%s)before:%s" ,(format "%s" fun-name) args)

;;                (let ((ret (apply f args)))
;;                    (message "(%s)after:%s" ,(format "%s" fun-name) ret)
;;                  ret)))
;;        (advice-add ',fun-name :around ',(intern (format "add-%s" fun-name)) '((depth . 100))))))
