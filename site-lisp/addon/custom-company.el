;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; company
;; 補完システム
;; from package
;;------------------------------------------------------------------------------

;; TAB キーの挙動をカスタマイズ
(defvar company-prefix)
(defun company--insert-candidate2 (candidate)
  (when (> (length candidate) 0)
	(setq candidate (substring-no-properties candidate))
	(if (eq (company-call-backend 'ignore-case) 'keep-prefix)
		(insert (company-strip-prefix candidate))
	  (if (equal company-prefix candidate)
		  (company-select-next)
		(delete-region (- (point) (length company-prefix)) (point))
		(insert candidate)))))

(defvar company-candidates)
(defvar company-common)
(defvar company-active-map)
(defun company-complete-common2 ()
  (interactive)
  (when (company-manual-begin)
	(if (and (not (cdr company-candidates))
			 (equal company-common (car company-candidates)))
		(company-complete-selection)
	  (company--insert-candidate2 company-common))))

(with-eval-after-load 'company
  (define-key company-active-map [tab] 'company-complete-common2)
  (define-key company-active-map [backtab] 'company-select-previous) ; おまけ

  ;; 基本設定
  (defvar company-idle-delay)  ;; 遅延
  (setq company-idle-delay 0.2)
  (defvar company-minimum-prefix-length) ;; 補完開始文字長
  (setq company-minimum-prefix-length 3)
  (defvar company-selection-wrap-around)  ;; 最下時に↓で最初に戻る
  (setq company-selection-wrap-around t))
