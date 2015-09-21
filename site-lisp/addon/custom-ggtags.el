;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; ggtags
;; タグジャンプツール. GNU Global を利用
;; from : package system
;;------------------------------------------------------------------------------
(when (eq system-type 'windows-nt)
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
  (advice-add 'ggtags-process-string :around 'ad-ggtags-process-string))
