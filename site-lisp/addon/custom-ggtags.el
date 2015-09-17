;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; ggtags
;; タグジャンプツール. GNU Global を利用
;; from : package system
;;------------------------------------------------------------------------------

(when (eq system-type 'windows-nt)
  (defun ad-ggtags-ensure-localname (file)
	"convert Windows path to UNIX path"
	(cygpath "-u" (and file (or (file-remote-p file 'localname) file))))
  (advice-add 'ggtags-ensure-localname :override 'ad-ggtags-ensure-localname)

  (defun ad-ggtags-process-string (orig-func program &rest args)
	"if execute global -pr command, convert UNIX path to Windows path"
	(let((arg (mapconcat 'identity (append (list program) args) nil)))
	  (cond ((string= "global-pr" arg)
			 (cygpath "-am" (apply orig-func program args)))
			(t (apply orig-func program args)))))
  (advice-add 'ggtags-process-string :around 'ad-ggtags-process-string))

;; use helm
(defvar ggtags-completing-read-function)
(setq ggtags-completing-read-function nil)
