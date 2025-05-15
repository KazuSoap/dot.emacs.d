;;; early-init.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; basic settings
;;==============================================================================
;;------------------------------------------------------------------------------
;; common-misc
;;------------------------------------------------------------------------------
;; garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; Maximum number of bytes to read from subprocess in a single chunk.
(setq read-process-output-max (* 1024 1024))

;; hide startup-message
(setq inhibit-startup-screen t)

;; change the message in *scratch* buffer
(setq initial-scratch-message nil)

;; major mode of *scratch* buffer
(setq initial-major-mode #'fundamental-mode)

;; don't beep
(setq ring-bell-function #'ignore)

;; don't make backup file
(setq auto-save-default nil  ; #*
      make-backup-files nil) ; *.~

;; mouse scroll
(setq mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control))))

;; Set "yes" to "y" and "no" to "n"
(setq use-short-answers t)

;; default major-mode
(setq-default major-mode #'text-mode)

;; indent with half-width spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; bidirectional text
(setq-default bidi-display-reordering nil) ; Set whether to allow bidirectional text.
;; (setq-default bidi-paragraph-direction 'left-to-right) ; Force text direction (default)

;; translate "C-h" to "Back Space"
(define-key key-translation-map [?\C-h] [?\C-?])

;;------------------------------------------------------------------------------
;; face & frame parameters
;;------------------------------------------------------------------------------
;; color-theme
(load-theme 'wheatgrass t)
(set-face-attribute 'mode-line nil :foreground "gray85" :background "#4a5459" :box nil)
(set-face-attribute 'fringe nil :background "black")

;; fontset
;; XLFD
;; https://wiki.archlinux.jp/index.php/X_Logical_Font_Description
;; https://qiita.com/j8takagi/items/01aecdd28f87cdd3cd2c
;; -maker-family-weight-slant-widthtype-style-px-height-resX-resY-spacing-width-registry-encoding
(create-fontset-from-fontset-spec
 (eval-when-compile
   (let ((fontset-elems
          '("-*-ricty diminished discord-bold-*-*-*-*-*-*-*-*-*-fontset-myricty"
            "ascii:-*-14-*"
            "unicode:-*")))
     (mapconcat #'identity fontset-elems ","))))

;; frame parameters
(setq default-frame-alist
      '((width . 100)
        (height . 30)
        (alpha . 85)
        (font . "fontset-myricty")))

;; show file path in title bar
(setq frame-title-format '((:eval (if (buffer-file-name) "%f" "%b")) " - Emacs"))

;; Threshold when splitting a window
;; (setq split-height-threshold nil) ; split by placing the new window below
(setq split-width-threshold nil) ; split by placing the new window right

;;------------------------------------------------------------------------------
;; global minor-mode
;;------------------------------------------------------------------------------
(tool-bar-mode -1) ; default on
(menu-bar-mode -1) ; default on
(fringe-mode -1) ; default on
(column-number-mode 1) ; default off
;; (line-number-mode -1) ; default on
;; (size-indication-mode 1) ; default off
(delete-selection-mode 1) ; default off
;; (add-hook 'after-init-hook (lambda () (blink-cursor-mode -1))) ; default on

;;------------------------------------------------------------------------------
;; windows-misc
;;------------------------------------------------------------------------------
(eval-when-compile
  (require 'exec-path-from-shell nil t)

  (defmacro setenv_cached-env-var (env-var-lst)
    (mapcar (lambda (x) `(setenv ,x ,(getenv x))) (eval env-var-lst)))

  ;; Windows Specific Settings
  (defmacro misc-nt ()
    (when (eq system-type 'windows-nt)
      (let* ((msys-root ;; Get the installation location of "msys2"
              (cond ((executable-find "reg")
                     (let* ((coding-system-for-read 'emacs-mule-dos) ;; Assume CRLF represents end-of-line, because of dos-command.
                            (reg_hkcu_uninstall_key "\"HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\"")
                            (reg_query_cmd (concat "reg query " reg_hkcu_uninstall_key " -v InstallLocation -s | findstr msys64")))
                       (expand-file-name
                        (nth 3 (split-string (shell-command-to-string reg_query_cmd) " +\\|\n")))))
                    (t ; else
                     (downcase (directory-file-name (string-trim (shell-command-to-string "cygpath -am /")))))))
             (msys-path ;; Set a minimum "PATH"
              (mapcar (lambda (subpath) (concat msys-root subpath "/bin/"))
                      '("/mingw64" "/usr" "/usr/local"))))
        (mapc (lambda (dir) (add-to-list 'exec-path dir)) msys-path))

      (setenv "PATH" (mapconcat #'identity exec-path path-separator))

      ;; Compile files that would natively compile before loading the "early-init" files and cause errors.
      (native-compile (locate-file "japan-util.el" load-path))

      (let ((setenv_list
             (cond ((require 'exec-path-from-shell nil t)
                    (let* ((cygpath
                            ;; cygpath for emacs lisp
                            (lambda (&optional option path)
                              (when path
                                (with-temp-buffer
                                  (call-process "cygpath" nil '(t nil) nil option path)
                                  (unless (bobp)
                                    (goto-char (point-min))
                                    (buffer-substring-no-properties (point) (line-end-position)))))))
                           (ad-exec-path-from-shell-setenv
                            ;; convert path format from unix style to win-nt style
                            (lambda (args)
                              (and (string= (car args) "PATH")
                                   (let ((path (replace-regexp-in-string ":/bin:" ":" (nth 1 args))))
                                     (setf (nth 1 args) (funcall cygpath "-amp" path))))
                              args)))
                      (advice-add 'exec-path-from-shell-setenv :filter-args ad-exec-path-from-shell-setenv))

                    ;; (mapc (lambda (x) (add-to-list 'exec-path-from-shell-variables x t))
                    ;;       '("PKG_CONFIG_PATH" "http_proxy" "https_proxy"))

                    (exec-path-from-shell-initialize)
                    (macroexpand '(setenv_cached-env-var exec-path-from-shell-variables)))
                   (t
                    (macroexpand '(setenv_cached-env-var '("PATH")))))))
        `(progn
           ;; Set environment variable
           (unless (getenv "SHLVL")
             ,@setenv_list
             (setq exec-path (append (parse-colon-path (getenv "PATH")) ',(last exec-path)))
             (setenv "SHLVL" "0")
             (setenv "SHELL" (setq shell-file-name "bash"))
             (setenv "LANG" ,(string-trim (shell-command-to-string "locale -uU")))
             (setenv "MSYSTEM" "MINGW64")
             (setenv "GIT_ASKPASS" "git-gui--askpass")
             (setenv "SSH_ASKPASS" "git-gui--askpass"))

           ;; Revert the 'null-device' modified in 'dos-w32.el'
           (setq null-device "/dev/null")

           ;; Set the default char-code in the following cases;
           ;; (1) when creating a new file,
           ;; (2) subprocess I/O,
           ;; (3) if not set elsewhere.
           (prefer-coding-system 'utf-8-unix)

           ;; allow a key sequence to be seen by Emacs instead of being grabbed by Windows
           ;; (setq w32-pass-lwindow-to-system nil)
           (setq w32-lwindow-modifier 'super)
           ;; (w32-register-hot-key [s-])
           (w32-register-hot-key [s-l])
           ))
      )))
(misc-nt)

;;------------------------------------------------------------------------------
;; load path
;;------------------------------------------------------------------------------
(let ((default-directory (eval-when-compile (locate-user-emacs-file "site-lisp"))))
  (normal-top-level-add-subdirs-to-load-path))

;;------------------------------------------------------------------------------
;; local functions
;;------------------------------------------------------------------------------
;; (eval-when-compile
;;   (defmacro start-my-shell-process-nt ()
;;     (when (eq system-type 'windows-nt)
;;       `(progn
;;          (fset 'start-my-shell-process
;;                (lambda (&rest args)
;;                  (unless (get-process "my-shell-process")
;;                    (start-process "my-shell-process" "my-shell" (getenv "SHELL"))
;;                    (set-process-query-on-exit-flag (get-process "my-shell-process") nil)
;;                    args)))

;;          (advice-add 'package-download-transaction :before 'start-my-shell-process)
;;          ))))
;; (start-my-shell-process-nt)

;; revert buffer
;; https://www.emacswiki.org/emacs/RevertBuffer
(eval-and-compile
  (fset 'my-revert-buffer-no-confirm
        (lambda (&optional force-reverting)
          (interactive "P")
          (cond ((or force-reverting (not (buffer-modified-p)))
                 (revert-buffer :ignore-auto :noconfirm))
                (t ; else
                 (error "The buffer has been modified")))
          )))
(global-set-key (kbd "<f5>") #'my-revert-buffer-no-confirm)

;; show "[EOB]" at the end of buffer
;; http://www.emacswiki.org/cgi-bin/wiki?HighlightEndOfBuffer
(let ((set-my-mark-eob
       (lambda ()
         (let ((existing-overlays (overlays-in (point-max) (point-max)))
               (eob-mark (make-overlay (point-max) (point-max) nil t t))
               (eob-text "[EOB]"))
           ;; Delete any previous EOB markers.  Necessary so that they don't
           ;; accumulate on calls to revert-buffer.
           (dolist (next-overlay existing-overlays)
             (when (overlay-get next-overlay 'eob-overlay)
               (delete-overlay next-overlay)))
           ;; Add a new EOB marker.
           ;; (put-text-property 0 (length eob-text) 'face '(foreground-color . "slate gray") eob-text)
           (overlay-put eob-mark 'eob-overlay t)
           (overlay-put eob-mark 'after-string eob-text)
           ))))
  (add-hook 'find-file-hook set-my-mark-eob))

;; ;; (fset 'my-expand-file-name
;; ;;       (lambda (f &rest args)
;; ;;         ;; (cond ((string-match-p "^\\(//\\|\\\\\\).+\\|^/\\(scp:\\|ssh:\\|rsync:\\|\\).+\\(@\\|!\\)|^/dtvsrv:.*" (car args))
;; ;;         ;; (cond ((string-match-p "^/dtvsrv:.*" (car args))
;; ;;         (cond ((string-match-p "^/dtvsrv:\\(c:\\|msys64\\)" (car args))
;; ;;         ;; (cond ((string-match-p "^/.*" (car args))
;; ;;                (message (format "0>%s" (car args)))
;; ;;                (let ((out (apply f args)))
;; ;;                  (message (format "1>%s" out))
;; ;;                  out
;; ;;                  ;; (car args)
;; ;;                  ))
;; ;;               (t ; else
;; ;;                (apply f args)))))
;; (fset 'my-expand-file-name
;;       (lambda (f &rest args)
;;         ;; (message (format "0>%s" args))
;;         (let ((out (apply f args)))
;;           (message (format "%s->%s" args out))
;;           (cond ((string-match-p "^/dtvsrv:\\(c:\\|msys64\\)" (car args))
;;                  ())
;;                 (t ; else
;;                  out))
;;           out)))

;; (advice-add 'expand-file-name :around 'my-expand-file-name)

;;==============================================================================
;; built-in package
;;==============================================================================
;;------------------------------------------------------------------------------
;; auto-insert
;; Insert template according to file type
;;------------------------------------------------------------------------------
;; (eval-when-compile (require 'autoinsert))
;; (with-eval-after-load 'autoinsert
;;   ;; Template directory
;;   (setq-default auto-insert-directory (eval-when-compile (expand-file-name "auto-insert" user-emacs-directory)))

;;   ;; Define variables to expand in the template
;;   (setq-default template-replacements-alists
;;                 `(("%file%"             . ,(lambda () (file-name-nondirectory (buffer-file-name))))
;;                   ("%file-without-ext%" . ,(lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
;;                   ("%include-guard%"    . ,(lambda () (format "INCLUDE_%s_H" (upcase (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))))))

;;   (fset 'my-template
;;         (lambda ()
;;           (time-stamp)
;;           (dolist (c (default-value 'template-replacements-alists))
;;             (goto-char (point-min))
;;             (while (search-forward (car c) nil t)
;;               (replace-match (funcall (cdr c)))))
;;           (goto-char (point-max))
;;           (message "done.")))

;;   ;; Switch templates according to file type
;;   (add-to-list 'auto-insert-alist '("\\.cpp$"   . ["template.cpp" my-template]))
;;   (add-to-list 'auto-insert-alist '("\\.h$"     . ["template.h" my-template]))
;;   (add-to-list 'auto-insert-alist '("Makefile$" . ["template.make" my-template])))

;; (add-hook 'find-file-not-found-functions #'auto-insert)

;;------------------------------------------------------------------------------
;; cua-mode
;; rectangle selection with C-Ret
;;------------------------------------------------------------------------------
(global-set-key (kbd "C-<return>") #'cua-rectangle-mark-mode)

;;------------------------------------------------------------------------------
;; GDB
;;------------------------------------------------------------------------------
;; ;; mode to open useful buffer
;; (setq-default gdb-many-windows t)

;; ;; hover mouse cursor over a variable to see its value
;; (add-hook 'gdb-mode-hook 'gud-tooltip-mode)

;; ;; show I/O buffer
;; (setq-default gdb-use-separate-io-buffer t)

;; ;; show value in mini buffer when set to t
;; (setq-default gud-tooltip-echo-area nil)

;;------------------------------------------------------------------------------
;; grep
;;------------------------------------------------------------------------------
(setq grep-command "rg -i --no-heading --color=always -nH --null -e ")
(setq-default grep-use-null-device nil)

;;------------------------------------------------------------------------------
;; Native Compile
;;------------------------------------------------------------------------------
(eval-when-compile
  (require 'comp)
  ;; Quitting emacs while native compilation in progress can leave zero byte
  ;; sized *.eln files behind. So delete such files when byte-compiling this file.
  (when (boundp 'native-comp-eln-load-path)
    (let ((eln-cache-dir (expand-file-name "eln-cache/" user-emacs-directory))
          (find-exec (executable-find "find")))
      (setcar native-comp-eln-load-path eln-cache-dir)
      (when find-exec
        (call-process find-exec nil nil nil eln-cache-dir
                      "-name" "*.eln" "-size" "0" "-delete" "-or"
                      "-name" "*.eln.tmp" "-size" "0" "-delete" "-or"
                      "-name" "*.eln.old" "-delete"))
      )))

;; suppress warnings and errors from asynchronous native compilation
(setq-default native-comp-async-report-warnings-errors nil)
(setq-default native-comp-async-jobs-number 8)
(setq native-comp-speed 3)

;;------------------------------------------------------------------------------
;; tab-bar-mode
;; frame-local tabs with named persistent window configurations
;;------------------------------------------------------------------------------
(tab-bar-history-mode +1)

(setq tab-bar-show 1)
(setq tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-close-button-show nil)
(setq tab-bar-tab-hints t)
(setq tab-bar-tab-name-function #'tab-bar-tab-name-current-with-count)

;; ;; remove background image
;; (add-text-properties 0 (length tab-bar-back-button) '(display t) tab-bar-back-button)
;; (add-text-properties 0 (length tab-bar-forward-button) '(display t) tab-bar-forward-button)

;; face
;; header-line
;; -> set with after-init-hook
;; selected
(set-face-attribute 'tab-bar-tab nil :foreground "yellow" :background "black" :box nil)
;; non-selected
(set-face-attribute 'tab-bar-tab-inactive nil :foreground "Gray72" :background "black")

;;------------------------------------------------------------------------------
;; uniquify
;; Distinguish the same file name
;;------------------------------------------------------------------------------
;; Specify the display format (default: 'post-forward-angle-brackets)
;; (setq-default uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Buffer name to ignore
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;------------------------------------------------------------------------------
;; vc-mode
;; version control
;;------------------------------------------------------------------------------
;; don't start "vc"
(setq vc-handled-backends nil)

;; removed "hook" related to "vc"
(remove-hook 'find-file-hook #'vc-refresh-state)
(remove-hook 'kill-buffer-hook #'vc-kill-buffer-hook)

;;==============================================================================
;; built-in packages not loaded immediately
;;==============================================================================
;;------------------------------------------------------------------------------
;; lpr
;; printer settings
;;------------------------------------------------------------------------------
(eval-when-compile
  (defmacro nt-printer ()
    (when (eq system-type 'windows-nt)
      '(with-eval-after-load 'lpr (require 'my-built-in))
      )))
(nt-printer)

;;------------------------------------------------------------------------------
;; package system
;;------------------------------------------------------------------------------
(with-eval-after-load 'package (require 'my-built-in))

;;==============================================================================
;; built-in hook
;;==============================================================================
;;------------------------------------------------------------------------------
;; after-init-hook
;;------------------------------------------------------------------------------
(eval-when-compile (require 'windmove))

(add-hook 'after-init-hook
          (lambda ()
            ;; show startup time in [ms]
            (message "Emacs loaded in %.3f ms" (* 1000 (string-to-number (emacs-init-time))))

            ;; face
            (set-face-attribute 'fixed-pitch nil :font "fontset-myricty")
            (set-face-attribute 'tab-bar nil :font "fontset-myricty" :foreground "Gray72" :background "black")

            ;; windmove
            ;; Move the split window with "modifier-key + arrow keys"
            (windmove-default-keybindings 'meta) ; modifier-key = Alt
            (setq windmove-wrap-around t) ; enable wrap-around
            ))

;;------------------------------------------------------------------------------
;; window-state-change-hook
;;------------------------------------------------------------------------------
(add-hook 'window-state-change-hook
          (lambda ()
            ;; disable vertical scrollbar on minibuffer
            (set-window-scroll-bars (minibuffer-window) nil nil)
            ))

;;; early-init.el ends here
