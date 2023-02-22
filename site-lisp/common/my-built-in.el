;;; my-built-in.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; emacs built in package
;;==============================================================================
;;------------------------------------------------------------------------------
;; display-line-numbers-mode
;;------------------------------------------------------------------------------
(with-eval-after-load 'display-line-numbers
  (set-face-attribute 'line-number nil :background "gray10")
  (set-face-attribute 'line-number-current-line nil :background "gray40"))

;;------------------------------------------------------------------------------
;; lpr
;; printer settings
;;------------------------------------------------------------------------------
(eval-when-compile
  (defmacro my-nt-lpr ()
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
(my-nt-lpr)

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
;; package system
;;------------------------------------------------------------------------------
(with-eval-after-load 'package
  (eval-when-compile (require 'package))

  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (setq package-menu-async nil
        package-quickstart t
        custom-file (eval-when-compile (concat user-emacs-directory "my-custom-file.el"))))

;;------------------------------------------------------------------------------
;; TRAMP(Transparent Remote Access Multiple Protocol)
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

(provide 'my-built-in)
;;; my-built-in.el ends here
