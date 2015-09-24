;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; fakecygpty
;; NTEmacs の仮想端末偽装
;; https://github.com/trueroad/fakecygpty
;;------------------------------------------------------------------------------

;; process-connection-type が nil で start-process がコールされるけれども、
;; fakecygpty を経由して起動したいプログラムの名称を列挙
(defvar fakecygpty-program-list)
(setq fakecygpty-program-list '(""))

;; fakecygpty を経由するかを判断してプログラムを起動する
(defun ad-start-process-to-fake (orig-fun &rest args)
  (when (and (nth 2 args)
             (or process-connection-type
                 (member (replace-regexp-in-string "\\.exe$"
                                                   ""
                                                   (file-name-nondirectory (nth 2 args)))
                         fakecygpty-program-list)))
    (push "fakecygpty" (nthcdr 2 args)))
  (apply orig-fun args))
(advice-add 'start-process :around 'ad-start-process-to-fake '((depth . 100)))

(defmacro fakecygpty-set-signal (fun-name send-key)
  `(progn
     (defun ,(intern (format "ad-%s" fun-name)) (orig-fun &rest args)
       (let ((process (or (car args)
                          (get-buffer-process (current-buffer)))))
         (if (string= (car (process-command process)) "fakecygpty")
             (process-send-string (car args) (kbd ,send-key))
           (apply orig-fun args))))
     (advice-add (quote ,fun-name) :around (quote ,(format "ad-%s" fun-name)))))

(fakecygpty-set-signal interrupt-process "C-c")
(fakecygpty-set-signal stop-process "C-z")
(fakecygpty-set-signal quit-process "C-\\")
(fakecygpty-set-signal process-send-eof "C-d")
