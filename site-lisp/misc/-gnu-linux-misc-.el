;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; IME
;;------------------------------------------------------------------------------
;; emacs-mozc
(setq default-input-method "japanese-mozc")

(with-eval-after-load 'mozc
  ;; (setq mozc-candidate-style 'overlay)
  ;; (require 'mozc-popup)
  ;; (setq mozc-candidate-style 'popup)
  (require 'mozc-cand-posframe)
  (setq mozc-candidate-style 'posframe)

  ;; 無変換キーのキーイベントを横取りする
  (fset 'ad-mozc-intercept-keys
        (lambda (f event)
          (cond
           ((member event (list 'muhenkan))
            ;; (message "%s" event) ;debug
            (mozc-clean-up-changes-on-buffer)
            (mozc-fall-back-on-default-binding event))
           (t ; else
            ;; (message "%s" event) ;debug
            (funcall f event)))))
  (advice-add 'mozc-handle-event :around 'ad-mozc-intercept-keys))

(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
(global-set-key (kbd "<henkan>") (lambda () (interactive) (activate-input-method default-input-method)))
(global-set-key (kbd "<muhenkan>") (lambda () (interactive) (deactivate-input-method)))

;; IME ON/OFF時のカーソルカラー
(add-hook 'input-method-activate-hook (lambda () (set-cursor-color "yellow")))
(add-hook 'input-method-deactivate-hook (lambda () (set-cursor-color "thistle")))

;;------------------------------------------------------------------------------
;; misc
;;------------------------------------------------------------------------------
(setq select-enable-clipboard t)

;;------------------------------------------------------------------------------
;; irony
;; A C/C++ minor mode powered by libclang
;; from package
;;------------------------------------------------------------------------------

(with-eval-after-load 'irony
  ;; irony-server-install に失敗する問題の修正用
  (defvar ex-irony--install-server-read-cmd "\\1"))
