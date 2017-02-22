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

;; msys2 で irony を動作させる設定
;; https://github.com/Sarcasm/irony-mode/wiki/Setting-up-irony-mode-on-Windows-using-Msys2-and-Mingw-Packages
(when (eq system-type 'windows-nt)
  ;; bash on Ubuntu on Windows の irony-server と共存するための設定
  (defun ad-irony--locate-server-executable (&rest args)
    (concat (car args) ".exe"))
  (advice-add 'irony--locate-server-executable :filter-return 'ad-irony--locate-server-executable)

  (setq w32-pipe-read-delay 0))
