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

;; lpr-bufferコマンド で notepad を開くようにする
(defvar print-region-function
      (lambda (start end _program
                     &optional _delete _destination _display
                     &rest _args)
        (let* ((procname (make-temp-name "w32-print-"))
                       (tempfile (expand-file-name procname temporary-file-directory))
                       (winfile
                        (if (fboundp 'cygwin-convert-file-name-to-windows)
                            (cygwin-convert-file-name-to-windows tempfile)
                          tempfile)))
          (let ((coding-system-for-write 'cp932-dos))
            (write-region start end winfile))
          (set-process-sentinel
           (start-process procname nil "notepad.exe" winfile)
           (lambda (_process _state)
             (when (file-exists-p winfile)
               (delete-file winfile)))))))

;; lpr-buffer を実行する
(global-set-key (kbd "C-c C-p")
                (lambda ()
                  (interactive)
                  (lpr-buffer)))
