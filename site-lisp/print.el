;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; 印刷設定
;;------------------------------------------------------------------------------
;; 文字化け防止
(defvar ps-multibyte-buffer)
(setq ps-multibyte-buffer 'non-latin-printer)

;; lpr-bufferコマンド で notepad を開くようにする
(defvar print-region-function)
(setq print-region-function
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
