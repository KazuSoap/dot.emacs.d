;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; ggtags
;; タグジャンプツール. GNU Global を利用
;; from : package system
;;------------------------------------------------------------------------------

;; cygwin or msys2 版 GNU Global で使うための設定
(defun ggtags-ensure-localname-Win (file)
  "convert windows path to unix path"
  (cygpath "-u" (and file (or (file-remote-p file 'localname) file))))
(advice-add 'ggtags-ensure-localname :override 'ggtags-ensure-localname-Win)

(defun ggtags-global-mode-Win ()
  "if use fakecygpty, must set compilation-disable-input to nil"
  (setq-local compilation-disable-input nil))
(advice-add 'ggtags-global-mode :after 'ggtags-global-mode-Win)

(defun ggtags-process-string-Win (orig-func program &rest args)
  "if execute global -pr command, convert UNIX path to Windows path"
  (let((arg (mapconcat #'identity (append (list program) args) nil)))
	(cond ((string= "global-pr" arg)
		   (cygpath "-m" (apply orig-func program args)))
		  (t (apply orig-func program args)))))
(advice-add 'ggtags-process-string :around 'ggtags-process-string-Win)

;; 特定のモードで自動的に有効化
(defun ggtags-mode-enable-hooks ()
  (ggtags-mode 1))
(dolist (hook '(c-mode-common-hook))
  (add-hook hook 'ggtags-mode-enable-hooks))

;; 特定のモードで無効化
;; (defun ggtags-mode-disable-hooks ()
;;   (ggtags-mode 0))
;; (dolist (hook '(lisp-interaction-mode-hook emacs-lisp-byte-code-mode-hook))
;;   (add-hook hook 'ggtags-mode-disable-hooks))

