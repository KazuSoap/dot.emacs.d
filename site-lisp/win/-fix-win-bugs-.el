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

    (defun ad-exec-path-from-shell-setenv (orig-fun &rest args)
      (when (and (string= (car args) "PATH") (fboundp 'cygpath))
        (setf (nth 1 args) (cygpath "-amp" (nth 1 args))))
      (apply orig-fun args))
    (advice-add 'exec-path-from-shell-setenv :around 'ad-exec-path-from-shell-setenv)))

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

(defun ad-magit-read-repository (orig-fun &optional args)
  (let ((ret_val (apply orig-fun args)))
    (if (file-directory-p ret_val) (file-truename ret_val))))
(advice-add 'magit-read-repository :around 'ad-magit-read-repository)

;;------------------------------------------------------------------------------
;; irony
;; A C/C++ minor mode powered by libclang
;; from package
;;------------------------------------------------------------------------------

;; msys2 で irony を動作させる設定
;; https://github.com/Sarcasm/irony-mode/wiki/Setting-up-irony-mode-on-Windows-using-Msys2-and-Mingw-Packages
(when (eq system-type 'windows-nt)
  (defun ad-irony--install-server-read-command (orig-func &rest args)
    "modify irony--install-server-read-command for msys2"
    (setenv "CC" "clang") (setenv "CXX" "clang++")
    (defvar irony-cmake-executable)
    (setcar args
            (replace-regexp-in-string
             (format "^\\(.*?%s\\)" (shell-quote-argument irony-cmake-executable))
             "\\1 -G'MSYS Makefiles' -DLIBCLANG_LIBRARY=/mingw64/bin/libclang.dll"
             (car args)))
    (apply orig-func args))
  (advice-add 'irony--install-server-read-command :around 'ad-irony--install-server-read-command)

  (setq w32-pipe-read-delay 0))
