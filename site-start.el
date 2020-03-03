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

  (defmacro windows-nt-core (username)
    "Windows Specific Settings"
    (when (eq system-type 'windows-nt)
      ;; Set the environment variable "HOME" & "SHELL"
      `(progn
         (setenv "SHELL" ,(concat msys-root "/usr/bin/bash"))
         (setenv "HOME" (concat ,(concat msys-root "/home/") ,username))

         (fset 'cygpath
               (lambda (&optional option path)
                 "cygpath for emacs lisp"
                 (when path
                   (with-temp-buffer
                     (call-process ,(concat msys-root "/usr/bin/cygpath") nil '(t nil) nil option path)
                     (unless (bobp)
                       (goto-char (point-min))
                       (buffer-substring-no-properties (point) (line-end-position)))))))))))

(eval-and-compile (windows-nt-core (getenv "USERNAME")))

;;------------------------------------------------------------------------------
;; garbage collection
;;------------------------------------------------------------------------------
(setq-default gc-cons-threshold (* 128 1024 1024))

;;------------------------------------------------------------------------------
;; face
;;------------------------------------------------------------------------------
;; color-theme
(load-theme 'wheatgrass t)
(set-face-attribute 'mode-line nil :foreground "gray85" :background "#4a5459")
(set-face-attribute 'fringe nil :background "black")

;;------------------------------------------------------------------------------
;; coding-system
;;------------------------------------------------------------------------------
;; デフォルトの文字コードを設定
;; 指定される文字コードは以下の項目
;; ① ファイルを新規作成した場合のデフォルト
;; ② サブプロセスでの IO
;; ③ 他の項目が指定されていない場合のデフォルト値
(prefer-coding-system 'utf-8-unix)

;; サブプロセスが出力する文字コードを判定して、process-coding-system の設定値を決定
;; (サブプロセスの "input-coding" を "undecided" にして実現)
(setq-default default-process-coding-system
              `(,(eval-when-compile
                   (let* ((input-coding (car (default-value 'default-process-coding-system)))
                          (input-eol-type (coding-system-eol-type input-coding)))
                     (cond ((eq input-eol-type 0) 'undecided-unix)
                           ((eq input-eol-type 1) 'undecided-dos)
                           ((eq input-eol-type 2) 'undecided-mac)
                           (t input-coding))))
                . utf-8-unix))

;;------------------------------------------------------------------------------
;; font
;;------------------------------------------------------------------------------
(set-frame-font "fontset-myricty")
(set-fontset-font (frame-parameter nil 'font) 'unicode (font-spec :family "Ricty Diminished Discord") nil 'append)

;;------------------------------------------------------------------------------
;; global minor-mode
;;------------------------------------------------------------------------------
(column-number-mode 1) ;; モードラインに列番号表示 (default off)
;; (line-number-mode -1) ;; モードラインに行番号表示 (default on)
;; (size-indication-mode 1) ;; モードラインにファイルサイズ表示 (default off)
(delete-selection-mode 1) ;; リージョン上書き (default off)

;;------------------------------------------------------------------------------
;; common-misc
;;------------------------------------------------------------------------------
;; "yes or no"を"y or n"に
(fset 'yes-or-no-p #'y-or-n-p)

;; C-hでBS, shift+C-hでHelp
(keyboard-translate ?\C-h ?\C-?) ; translate `C-h' to DEL
(keyboard-translate ?\C-? ?\C-h)  ; translate DEL to `C-h'

;; ファイルのフルパスをタイトルバーに表示
(setq-default frame-title-format (format "%%f - Emacs"))

;; beep音 off
(setq-default ring-bell-function #'ignore)

;; don't make BackUp file
(setq-default auto-save-default nil) ;; #*
(setq-default make-backup-files nil) ;; *.~

;; インデントは tab でなく 半角スペース
(setq-default indent-tabs-mode nil)

;; 双方向テキスト
(setq-default bidi-display-reordering nil) ;; 双方向テキスト可否
(setq-default bidi-paragraph-direction 'left-to-right) ;; テキスト方向を強制

;; mouse scroll
(setq-default mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control))))
