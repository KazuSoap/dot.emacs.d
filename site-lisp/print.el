;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; 印刷設定
;;------------------------------------------------------------------------------
;;ak2prでEmacs のバッファの内容を印刷する
;;(ただし日本語は sjis のみ? utf-8 非対応)
;(setq lpr-command "d:/Program Files (x86)/ak2pr/bin/ak2pr.exe")
;(setq lpr-switches '())
;(defvar lpr-add-switches t)
;(defvar lpr-command-switches '())

;;NTEmacsからnotepadを使って印刷
(setq print-region-function
      (lambda (start end
                     &optional _lpr-prog
                     _delete-text _buf _display
                     &rest _rest)
        (let* ((procname (make-temp-name "w32-print-"))
               (tempfile
                (subst-char-in-string
                 ?/ ?\\
                 (expand-file-name procname temporary-file-directory)))
               (coding-system-for-write 'sjis-dos))
          (write-region start end tempfile)
          (set-process-sentinel
           (start-process procname nil "notepad.exe" tempfile)
           (lambda (process _event)
             (let ((tempfile
                    (expand-file-name (process-name process)
                                      temporary-file-directory)))
               (when (file-exists-p tempfile)
                 (delete-file tempfile))))))))
