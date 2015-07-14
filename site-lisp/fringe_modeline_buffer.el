;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; fringe & modeline & buffer
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; fringe
;;------------------------------------------------------------------------------
;;-- linum customize --;;
;; linumのカスタマイズ
(defvar linum-format)
(setq linum-format "%4d\u2502") ;; 行番号のフォーマット

;; face
(custom-set-faces
 '(linum ((t (:background "black" :foreground "gray" :height 0.8 :underline nil)))))

;; ;; 行番号の表示遅延の修正
(defvar linum-delay)
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))

;; linum を有効にする mode
(dolist (hook '(text-mode-hook emacs-lisp-mode-hook sh-mode-hook
				c-mode-hook c++-mode-hook makefile-mode-hook))
  (add-hook hook '(lambda ()
					(linum-mode 1))))

;; linum を無効にする mode
(dolist (hook '(lisp-interaction-mode-hook emacs-lisp-byte-code-mode-hook))
  (add-hook hook '(lambda ()
					(linum-mode 0))))

;;------------------------------------------------------------------------------
;; modeline
;;------------------------------------------------------------------------------
;;-- IME customize --;;
;; IMEのカスタマイズ
(setq default-input-method "W32-IME") ;;標準IMEの設定

;; Windows IME の ON:[あ]/OFF:[Aa] をモードラインに表示
(setq-default w32-ime-mode-line-state-indicator "[Aa]")
(setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

;; IME の初期化
(w32-ime-initialize)

;; バッファ切り替え時にIME状態を引き継ぐ
(setq w32-ime-buffer-switch-p t)

;; IME ON/OFF時のカーソルカラー
(add-hook 'input-method-activate-hook
          (lambda() (set-cursor-color "#AA0000")))
(add-hook 'input-method-inactivate-hook
          (lambda() (set-cursor-color "yellow")))

;; IMEの制御（yes/noをタイプするところでは IME をオフにする）
(wrap-function-to-control-ime 'universal-argument t nil)
(wrap-function-to-control-ime 'read-string nil nil)
(wrap-function-to-control-ime 'read-char nil nil)
(wrap-function-to-control-ime 'read-from-minibuffer nil nil)
(wrap-function-to-control-ime 'y-or-n-p nil nil)
(wrap-function-to-control-ime 'yes-or-no-p nil nil)
(wrap-function-to-control-ime 'map-y-or-n-p nil nil)

;;; time -----------------------------------------------------------------------
;; 時刻の表示
;; emacs default
(require 'time)
(setq display-time-string-forms
   '((format "%s/%s/%s(%s) %s:%s "
			 year month day dayname 24-hours minutes)
	 load))
(setq display-time-24hr-format t)
(display-time)

;;------------------------------------------------------------------------------
;; buffer
;;------------------------------------------------------------------------------
;;-- buffer customize --;;
;; bufferのカスタマイズ

;;-- buffer --;;
(set-default 'truncate-lines nil) ;; buffer画面外文字の切り詰め表示
(setq truncate-partial-width-windows t) ;; window縦分割時のbuffer画面外文字の切り詰め表示

;;-- cursor --;;
(setq blink-cursor-mode 1) ;; cursor点滅表示
;(setq scroll-preserve-screen-position t) ;; scroll時のcursor位置の維持
;(setq next-screen-context-lines 1) ;; 画面scroll時の重複行数
;(setq scroll-conservatively 35) ;; scroll行数(一行ごとのscroll)
;(setq scroll-margin 0) ;;同上
;(setq scroll-step 1) ;;同上

;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

;; 1 line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; 2 line at a time
(global-set-key [double-wheel-up] '(lambda () "" (interactive) (scroll-down 2)))
(global-set-key [double-wheel-down] '(lambda () "" (interactive) (scroll-up 2)))

;; 3 line at a time
(global-set-key [triple-wheel-up] '(lambda () "" (interactive) (scroll-down 4)))
(global-set-key [triple-wheel-down] '(lambda () "" (interactive) (scroll-up 4)))

;;; whitespace -----------------------------------------------------------------
;; 不可視文字の可視化
;; emacs default
(require 'whitespace)
(global-whitespace-mode 1)

;; see whitespace.el for more details
(setq whitespace-style '(face tabs tab-mark newline newline-mark spaces space-mark trailing))
;; 表示の変更
(setq whitespace-display-mappings
	  '(;; space → " "
		(space-mark ?\xA0 [?\u00A4] [?_])
		(space-mark ?\x8A0 [?\x8A4] [?_])
		(space-mark ?\x920 [?\x924] [?_])
		(space-mark ?\xE20 [?\xE24] [?_])
		(space-mark ?\xF20 [?\xF24] [?_])
		;; full-width-space → "□"
		(space-mark ?\u3000 [?\u25a1] [?_ ?_])
		;; tab → "»"
		(tab-mark     ?\t    [?\xBB ?\t]   [?\\ ?\t])
		;; newline → "｣"
		(newline-mark ?\n    [?\uFF63 ?\n] [?$ ?\n])))
 ;; 以下の正規表現にマッチするものを"space"と認識
(setq whitespace-space-regexp "\\(\u3000+\\)")

(custom-set-faces
 '(whitespace-space ((t (:background nil :foreground "GreenYellow"))))
 '(whitespace-tab ((t (:background nil :foreground "LightSkyBlue" :underline t))))
 '(whitespace-newline ((t (:background nil :foreground "DeepSkyBlue"))))
 '(whitespace-trailing ((t (:background "DeepPink" :foreground nil)))))

;;; my-mark-eob ----------------------------------------------------------------
;; バッファの最後に "[EOB]" を表示
;; http://www.emacswiki.org/cgi-bin/wiki?HighlightEndOfBuffer
(defun my-mark-eob ()
  (let ((existing-overlays (overlays-in (point-max) (point-max)))
	(eob-mark (make-overlay (point-max) (point-max) nil t t))
	(eob-text "[EOB]"))
    ;; Delete any previous EOB markers.  Necessary so that they don't
    ;; accumulate on calls to revert-buffer.
    (dolist (next-overlay existing-overlays)
      (if (overlay-get next-overlay 'eob-overlay)
	  (delete-overlay next-overlay)))
    ;; Add a new EOB marker.
    (put-text-property 0 (length eob-text)
                       'face '(foreground-color . "slate gray") eob-text)
    (overlay-put eob-mark 'eob-overlay t)
    (overlay-put eob-mark 'after-string eob-text)))
(add-hook 'find-file-hook 'my-mark-eob)

;;; uniquify -------------------------------------------------------------------
;; 同一buffer名にディレクトリ付与
;; emacs default
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;------------------------------------------------------------------------------
