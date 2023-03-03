;;; my-built-in.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; emacs built in package
;;==============================================================================
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
;; package system
;;------------------------------------------------------------------------------
(eval-when-compile (require 'package))
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (setq package-menu-async nil
        package-quickstart t
        custom-file (eval-when-compile (locate-user-emacs-file "my-custom-file.el"))))

(provide 'my-built-in)
;;; my-built-in.el ends here
