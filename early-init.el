;;; -*- coding: utf-8; lexical-binding: t -*-

;;==============================================================================
;; basic settings
;;==============================================================================
;;------------------------------------------------------------------------------
;; windows-misc
;;------------------------------------------------------------------------------
(eval-when-compile
  ;; Get the installation location of "msys2"
  (defconst msys-root
    (let* ((coding-system-for-read 'emacs-mule-dos) ;; Assume CRLF represents end-of-line, because of dos-command.
           (reg_hkcu_uninstall_key "\"HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\"")
           (reg_query_cmd (concat "reg query " reg_hkcu_uninstall_key " -v InstallLocation -s | findstr msys64")))
      (ignore-errors
        (expand-file-name
          (nth 3 (split-string (shell-command-to-string reg_query_cmd) " +\\|\n"))))))

  ;; Windows Specific Settings
  (defmacro misc-nt ()
    (when (eq system-type 'windows-nt)
      `(progn
         ;; Set environment variable
         (setenv "HOME" (concat ,(concat msys-root "/home/") user-login-name))
         (setenv "SHELL" ,(concat msys-root "/usr/bin/bash"))
         (setenv "MSYSTEM" "MINGW64")
         (or (getenv "SHLVL") (setenv "SHLVL" "0"))
         (setq shell-file-name ,(getenv "SHELL"))

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
         ))))

;;------------------------------------------------------------------------------
;; common-misc
;;------------------------------------------------------------------------------
;; garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; windows-misc
(misc-nt)

;; Maximum number of bytes to read from subprocess in a single chunk.
(setq read-process-output-max (eval-when-compile (* 1024 1024)))

;; hide startup-message
(setq inhibit-startup-screen t)

;; change the message in *scratch* buffer
(setq initial-scratch-message nil)

;; show file path in title bar
(setq frame-title-format '((:eval (if (buffer-file-name) "%f" "%b")) " - Emacs"))

;; don't beep
(setq ring-bell-function #'ignore)

;; don't make backup file
(setq auto-save-default nil  ; #*
      make-backup-files nil) ; *.~

;; Threshold when splitting a window
;; (setq split-height-threshold nil) ; split by placing the new window below
(setq split-width-threshold nil) ; split by placing the new window right

;; mouse scroll
(setq mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control))))

;; major mode of *scratch* buffer
(setq initial-major-mode #'fundamental-mode)

;; default major-mode
(setq-default major-mode #'text-mode)

;; indent with half-width spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; bidirectional text
(setq-default bidi-display-reordering nil) ; Set whether to allow bidirectional text.
;; (setq-default bidi-paragraph-direction 'left-to-right) ; Force text direction (default)

;; Set "yes" to "y" and "no" to "n"
(fset 'yes-or-no-p #'y-or-n-p)

;; translate "C-h" to "Back Space"
(define-key key-translation-map [?\C-h] [?\C-?])

(add-hook 'window-state-change-hook
          (lambda ()
            ;; disable vertical scrollbar on minibuffer
            (set-window-scroll-bars (minibuffer-window) nil nil)
            ))

(add-hook 'after-init-hook
          (lambda ()
            ;; 128mb
            (setq gc-cons-threshold (eval-when-compile (* 128 1024 1024)))

            ;; show startup time in [ms]
            (message "Emacs loaded in %.3f ms"
                     (* 1000 (string-to-number (emacs-init-time))))
            ))

;;------------------------------------------------------------------------------
;; face & frame parameters
;;------------------------------------------------------------------------------
;; color-theme
(load-theme 'wheatgrass t)
(set-face-attribute 'mode-line nil :foreground "gray85" :background "#4a5459" :box nil)
(set-face-attribute 'fringe nil :background "black")
(set-face-attribute 'fixed-pitch nil :family "ricty diminished discord")

;; fontset
;; XLFD
;; https://wiki.archlinux.jp/index.php/X_Logical_Font_Description
;; https://qiita.com/j8takagi/items/01aecdd28f87cdd3cd2c
;; -maker-family-weight-slant-widthtype-style-px-height-resX-resY-spacing-width-registry-encoding
(create-fontset-from-fontset-spec
 (eval-when-compile
   (let ((fontset-base
          (cond ((eq system-type 'windows-nt) ; if
                 "-outline-ricty diminished discord-bold-normal-normal-mono-*-*-*-*-c-*-fontset-myricty")
                (t ; else
                 "-PfEd-ricty diminished discord-bold-normal-normal-*-*-*-*-*-m-0-fontset-myricty")))
         (ascii-font "ascii:-*-*-*-*-*-*-14-*-*-*-*-*-iso10646-1")
         (unicode-font "unicode:-*-*-*-*-*-*-*-*-*-*-*-*-iso10646-1"))
     (mapconcat #'identity `(,fontset-base ,ascii-font ,unicode-font) ","))))

;; frame parameters
(setq default-frame-alist
      '((width . 100)
        (height . 30)
        (alpha . 85)
        (font . "fontset-myricty")))

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
;; load path
;;------------------------------------------------------------------------------
(let ((default-directory (eval-when-compile (concat user-emacs-directory "site-lisp"))))
  (normal-top-level-add-subdirs-to-load-path))

;;------------------------------------------------------------------------------
;; local functions
;;------------------------------------------------------------------------------
;; cygpath for emacs lisp
(eval-when-compile
  (defmacro cygpath-nt ()
    (when (eq system-type 'windows-nt)
      `(progn
         (fset 'cygpath
               (lambda (&optional option path)
                 (when path
                   (with-temp-buffer
                     (call-process ,(concat msys-root "/usr/bin/cygpath") nil '(t nil) nil option path)
                     (unless (bobp)
                       (goto-char (point-min))
                       (buffer-substring-no-properties (point) (line-end-position)))))))
         ))))
(cygpath-nt)

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
(fset 'my-revert-buffer-no-confirm
      (lambda (&optional force-reverting)
        (interactive "P")
        (if (or force-reverting (not (buffer-modified-p)))
            (revert-buffer :ignore-auto :noconfirm)
          (error "The buffer has been modified"))))
(global-set-key (kbd "<f5>") 'my-revert-buffer-no-confirm)

;; show "[EOB]" at the end of buffer
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
;; emacs built in package
;;==============================================================================
;;------------------------------------------------------------------------------
;; auto-insert
;; Insert template according to file type
;;------------------------------------------------------------------------------
;; (eval-when-compile (require 'autoinsert))
;; (with-eval-after-load 'autoinsert
;;   ;; Template directory
;;   (setq-default auto-insert-directory (eval-when-compile (expand-file-name (concat user-emacs-directory "auto-insert"))))

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
(global-set-key (kbd "C-<return>") 'cua-rectangle-mark-mode)

;;------------------------------------------------------------------------------
;; display-line-numbers-mode
;;------------------------------------------------------------------------------
(with-eval-after-load 'display-line-numbers
  (set-face-attribute 'line-number nil :background "gray10")
  (set-face-attribute 'line-number-current-line nil :background "gray40"))

;;------------------------------------------------------------------------------
;; ediff
;;------------------------------------------------------------------------------
(with-eval-after-load 'ediff
  (eval-when-compile (require 'ediff))
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (set-face-attribute 'ediff-even-diff-A nil :background "gray20")
  (set-face-attribute 'ediff-even-diff-B nil :background "gray20")
  (set-face-attribute 'ediff-odd-diff-A  nil :background "gray20")
  (set-face-attribute 'ediff-odd-diff-B  nil :background "gray20"))

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
;; lpr
;; printer settings
;;------------------------------------------------------------------------------
(eval-when-compile
  (defmacro nt-printer ()
    (when (eq system-type 'windows-nt)
      (require 'lpr)
      `(progn
         ;; Open notepad with lpr-buffer command
         (with-eval-after-load 'lpr
           (setq print-region-function
                 (lambda (start end _program &optional _delete _destination _display &rest _args)
                   (let* ((procname (make-temp-name "w32-print-"))
                          (winfile (expand-file-name procname temporary-file-directory)))
                     (write-region start end winfile)
                     (set-process-sentinel
                      (start-process procname nil "notepad.exe" winfile)
                      (lambda (_process _state)
                        (when (file-exists-p winfile)
                          (delete-file winfile))))))))))))
(nt-printer)

;;------------------------------------------------------------------------------
;; tab-bar-mode
;; frame-local tabs with named persistent window configurations
;;------------------------------------------------------------------------------
(fset 'my-tab-bar-mode-setup
      (lambda ()
        (tab-bar-history-mode +1)

        (setq tab-bar-show 1)
        (setq tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
        (setq tab-bar-close-button-show nil)
        (setq tab-bar-tab-hints t)
        (setq tab-bar-tab-name-function #'tab-bar-tab-name-current-with-count)

        ;; remove background image
        (add-text-properties 0 (length tab-bar-back-button) '(display t) tab-bar-back-button)
        (add-text-properties 0 (length tab-bar-forward-button) '(display t) tab-bar-forward-button)

        ;; face
        ;; header-line
        (set-face-attribute 'tab-bar nil :font "fontset-myricty" :foreground "Gray72" :background "black")
        ;; selected
        (set-face-attribute 'tab-bar-tab nil :foreground "yellow" :background "black" :box nil)
        ;; non-selected
        (set-face-attribute 'tab-bar-tab-inactive nil :foreground "Gray72" :background "black")))
(add-hook 'tab-bar-mode-hook 'my-tab-bar-mode-setup)

;;------------------------------------------------------------------------------
;; TRAMP(TransparentRemoteAccessMultipleProtocol)
;; edit remoto file from local emacs
;;------------------------------------------------------------------------------
(with-eval-after-load 'tramp
  (eval-when-compile (require 'tramp))
  (declare-function tramp-change-syntax "tramp")
  (tramp-change-syntax 'simplified) ; Emacs 26.1 or later
  (setq tramp-encoding-shell "bash")

  ;; When connecting to a remote shell, disable the LC_ALL setting to prevent garbled Japanese characters
  ;; http://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-processes.html#Running%20a%20debugger%20on%20a%20remote%20host
  (let ((process-environment (default-value 'tramp-remote-process-environment)))
    (setenv "LC_ALL" nil)
    (setq tramp-remote-process-environment process-environment)))

;;------------------------------------------------------------------------------
;; uniquify
;; Distinguish the same file name
;;------------------------------------------------------------------------------
;; Specify the display format (default: 'post-forward-angle-brackets)
;; (setq-default uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Buffer name to ignore
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;------------------------------------------------------------------------------
;; whitespace-mode
;; Visualize invisible characters
;;------------------------------------------------------------------------------
(with-eval-after-load 'whitespace
  (eval-when-compile (require 'whitespace))
  ;; delete trailing whitespace when saving
  (add-hook 'before-save-hook #'delete-trailing-whitespace)

  ;; list of invisible characters to visualize
  (setq whitespace-style '(face trailing tabs spaces newline space-mark tab-mark newline-mark))

  ;; display mapping of invisible characters
  (setq whitespace-display-mappings
        '(;; space > " "
          (space-mark   ?\xA0   [?\u00A4]     [?_])
          (space-mark   ?\x8A0  [?\x8A4]      [?_])
          (space-mark   ?\x920  [?\x924]      [?_])
          (space-mark   ?\xE20  [?\xE24]      [?_])
          (space-mark   ?\xF20  [?\xF24]      [?_])
          ;; full-width-space > "□"
          (space-mark   ?\u3000 [?\u25a1]     [?_ ?_])
          ;; tab > "»" with underline
          (tab-mark     ?\t     [?\xBB ?\t]   [?\\ ?\t])
          ;; newline > "｣"
          (newline-mark ?\n     [?\uFF63 ?\n] [?$ ?\n])))

  ;; recognize "space" if it matches the following regular expression
  (setq whitespace-space-regexp "\\(\u3000+\\)")

  ;; face
  (set-face-attribute 'whitespace-space nil :foreground "GreenYellow" :background "black")
  (set-face-attribute 'whitespace-tab nil :foreground "LightSkyBlue" :background "black" :underline t)
  (set-face-attribute 'whitespace-newline nil :foreground "DeepSkyBlue")
  (set-face-attribute 'whitespace-trailing nil :background "DeepPink"))

;;------------------------------------------------------------------------------
;; windmove
;; Move the split window with "modifier-key + arrow keys"
;;------------------------------------------------------------------------------
(eval-when-compile (require 'windmove))
(fset 'activate-windmove
      (lambda ()
        (unless (boundp 'windmove-wrap-around)
          (windmove-default-keybindings 'meta) ; modifier-key = Alt
          (setq windmove-wrap-around t) ; enable wrap-around
          (remove-hook 'window-configuration-change-hook 'activate-windmove)
          (fmakunbound 'activate-windmove))))
(add-hook 'window-configuration-change-hook 'activate-windmove)

;;------------------------------------------------------------------------------
;; vc-mode
;; version control
;;------------------------------------------------------------------------------
;; don't start "vc"
(setq vc-handled-backends nil)

;; removed "hook" related to "vc"
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)
