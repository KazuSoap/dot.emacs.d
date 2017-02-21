;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; fringe & modeline & buffer
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; fringe
;;------------------------------------------------------------------------------
;;-- linum customize --;;
(defvar nlinum-mode)
(defvar nlinum-format)
(defun my-nlinum-mode-hook ()
  (when nlinum-mode
    (setq-local nlinum-format
                (concat "%" (number-to-string
                             ;; Guesstimate number of buffer lines.
                             (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
                        "d\u007c"))))

(with-eval-after-load 'nlinum
  (add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook))

;;------------------------------------------------------------------------------
;; modeline
;;------------------------------------------------------------------------------
;;-- modeline customize --;;
(line-number-mode 1) ;; モードラインに行番号表示
(column-number-mode 1) ;; モードラインに列番号表示
(size-indication-mode -1) ;; モードラインにファイルサイズ表示

;; 総行数表示
;; (setf mode-line-position '(:eval (format "%d" (count-lines (point-max) (point-min)))))

;;; time -----------------------------------------------------------------------
;; 時刻の表示
;; emacs default

;; (setq display-time-string-forms
;;    '((format "%s/%s/%s(%s) %s:%s "
;;  year month day dayname 24-hours minutes)
;;  load))
;; (setq display-time-24hr-format t)
;; (display-time)

;;------------------------------------------------------------------------------
;; buffer
;;------------------------------------------------------------------------------
;;-- buffer customize --;;
;;-- cursor --;;
(setq blink-cursor-mode 1) ;; cursor点滅表示
(setq scroll-preserve-screen-position t) ;; scroll時のcursor位置の維持
(setq next-screen-context-lines 1) ;; 画面scroll時の重複行数
(setq scroll-conservatively 1) ;; scroll行数(一行ごとのscroll)
(setq scroll-margin 1) ;;同上
(setq scroll-step 1) ;;同上

;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

;; 1 line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; 2 line at a time
(global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down 2)))
(global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up 2)))

;; 3 line at a time
(global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down 4)))
(global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up 4)))

;;; whitespace -----------------------------------------------------------------
;; 不可視文字の可視化
;; emacs default

(with-eval-after-load 'whitespace
  ;; 保存時に行末の空白を削除する
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (defvar whitespace-style)
  (setq whitespace-style
        '(face tabs tab-mark newline newline-mark spaces space-mark trailing))

  ;; 表示の変更
  (defvar whitespace-display-mappings)
  (setq whitespace-display-mappings
        '(;; space → " "
          (space-mark ?\xA0 [?\u00A4] [?_])
          (space-mark ?\x8A0 [?\x8A4] [?_])
          (space-mark ?\x920 [?\x924] [?_])
          (space-mark ?\xE20 [?\xE24] [?_])
          (space-mark ?\xF20 [?\xF24] [?_])
          ;; full-width-space → "□"
          (space-mark ?\u3000 [?\u25a1] [?_ ?_])
          ;; tab → "»" with underline
          (tab-mark     ?\t    [?\xBB ?\t]   [?\\ ?\t])
          ;; newline → "｣"
          (newline-mark ?\n    [?\uFF63 ?\n] [?$ ?\n])))

  ;; 以下の正規表現にマッチするものを"space"と認識
  (defvar whitespace-space-regexp)
  (setq whitespace-space-regexp "\\(\u3000+\\)"))

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
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;------------------------------------------------------------------------------