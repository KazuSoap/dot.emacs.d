;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; ggtags
;; タグジャンプツール. GNU Global を利用
;; from : package system
;;------------------------------------------------------------------------------

;; cygwin or msys2 版 GNU Global で使うための設定
(when (eq system-type 'windows-nt)
  (defun ggtags-ensure-localname-Win (file)
	"convert Windows path to UNIX path"
	(cygpath "-u" (and file (or (file-remote-p file 'localname) file))))
  (advice-add 'ggtags-ensure-localname :override 'ggtags-ensure-localname-Win)

  (defun ggtags-process-string-Win (orig-func program &rest args)
	"if execute global -pr command, convert UNIX path to Windows path"
	(let((arg (mapconcat 'identity (append (list program) args) nil)))
	  (cond ((string= "global-pr" arg)
			 (cygpath "-m" (apply orig-func program args)))
			(t (apply orig-func program args)))))
  (advice-add 'ggtags-process-string :around 'ggtags-process-string-Win))

;; use helm
(defvar ggtags-completing-read-function)
(setq ggtags-completing-read-function nil)
