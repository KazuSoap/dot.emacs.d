;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; local functions
;;------------------------------------------------------------------------------
(defun display-loading-error-message (filename)
  "Display Loading Error Message"
  (let ((buffer (get-buffer-create "*Loading Error*")))
	(set-buffer buffer)
	(insert "cannot load \"" filename "\"\n")
	(display-buffer (current-buffer)))
  )

