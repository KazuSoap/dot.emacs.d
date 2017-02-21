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
(with-eval-after-load "-shell-"
  (require 'fakecygpty)
  (fakecygpty-activate))

;;------------------------------------------------------------------------------
;; ggtags
;; タグジャンプツール. GNU Global を利用
;; from : package system
;;------------------------------------------------------------------------------
(defun ad-ggtags-ensure-localname (orig-func &rest args)
  "convert Windows path to UNIX path"
  (if (fboundp 'cygpath)
      (cygpath "-u" (apply orig-func args))
    (apply orig-func args)))
(advice-add 'ggtags-ensure-localname :around 'ad-ggtags-ensure-localname)

(defun ad-ggtags-process-string (orig-func &rest args)
  "if execute global -pr command, convert UNIX path to Windows path"
  (if (and (string= "global" (car args))
           (string= "-pr" (nth 1 args))
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
  (when (file-directory-p args)
    (file-truename args)))
(advice-add 'magit-read-repository :filter-return 'ad-magit-read-repository)

;;------------------------------------------------------------------------------
;; irony
;; A C/C++ minor mode powered by libclang
;; from package
;;------------------------------------------------------------------------------

;; msys2 で irony を動作させる設定
;; https://github.com/Sarcasm/irony-mode/wiki/Setting-up-irony-mode-on-Windows-using-Msys2-and-Mingw-Packages
(when (eq system-type 'windows-nt)
  ;; bash on Ubuntu on Windows の irony-server と共存するための設定
  (defvar irony-server-install-prefix)

  (defun ad-irony--locate-server-executable ()
    (let* ((irony--server-name "irony-server.exe")
           (exe (expand-file-name (concat "bin/" irony--server-name) irony-server-install-prefix)))
      (condition-case err
          (let ((irony-server-version (car (process-lines exe "--version"))))
            (if (and (string-match "^irony-server version " irony-server-version)
                     (version= (irony-version)
                               (substring irony-server-version
                                          (length "irony-server version "))))
                ;; irony-server is working and up-to-date!
                exe
              (message "irony-server version mismatch: %s"
                       (substitute-command-keys
                        "type `\\[irony-install-server]' to reinstall"))
              nil))
        (error
         (if (file-executable-p exe)
             ;; failed to execute due to a runtime problem, i.e: libclang.so isn't
             ;; in the ld paths
             (message "error: irony-server is broken, good luck buddy! %s"
                      (error-message-string err))
           ;; irony-server doesn't exists, first time irony-mode is used? inform
           ;; the user about how to build the executable
           (message "%s"
                    (substitute-command-keys
                     "Type `\\[irony-install-server]' to install irony-server")))
         ;; return nil on error
         nil))))
  (advice-add 'irony--locate-server-executable :override 'ad-irony--locate-server-executable)

  (setq w32-pipe-read-delay 0))
