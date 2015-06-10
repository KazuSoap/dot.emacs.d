;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; 起動時間測定
;;------------------------------------------------------------------------------
;;; calculate bootup time/ スピード狂に捧ぐ.
;; 目標: 3000ms 圏内
(defun message-startup-time ()
    (message
     "Emacs loaded in %dms"
     (/ (- (+ (car (cdr (cdr after-init-time))) (* 1000000 (car (cdr after-init-time))))
           (+ (car (cdr (cdr before-init-time))) (* 1000000 (car (cdr before-init-time)))))
        1000)))
  (add-hook 'after-init-hook 'message-startup-time)
