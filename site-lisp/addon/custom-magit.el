;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; magit
;; git クライアント
;; from package
;;------------------------------------------------------------------------------

(when (eq system-type 'windows-nt)
  (defun ad-magit-read-repository (orig-fun &rest args)
    (let ((ret_val (apply orig-fun args)))
      (if (file-directory-p ret_val) (file-truename ret_val))))
  (advice-add 'magit-read-repository :around 'ad-magit-read-repository))
