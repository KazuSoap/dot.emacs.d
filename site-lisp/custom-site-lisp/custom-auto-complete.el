;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; auto-complete
;; 補完システム
;; from package
;;------------------------------------------------------------------------------

;; global-set-key
(global-set-key (kbd "M-<RET>") 'auto-complete-mode)

;; 特定のモードで自動的に有効化
(defun auto-complete-mode-enable-hooks ()
  (auto-complete-mode t))

(dolist (hook '(text-mode-hook emacs-lisp-mode-hook
				sh-mode-hook makefile-mode-hook
				c-mode-common-hook))
  (add-hook hook 'auto-complete-mode-enable-hooks))

;; 特定のモードで無効化
(defun auto-complete-mode-disable-hooks ()
  (auto-complete-mode 0))

(dolist (hook '(lisp-interaction-mode-hook emacs-lisp-byte-code-mode-hook))
  (add-hook hook 'auto-complete-mode-disable-hooks))

(with-eval-after-load 'auto-complete
  (ac-config-default)

  ;; ファイル名の補完をしない
  (defvar ac-sources)
  (defun ac-common-setup ()
	(setq ac-sources '(
					   ac-source-functions
					   ac-source-variables
					   ac-source-symbols
					   ac-source-features
					   ;;ac-source-filename
					   ac-source-abbrev
					   ac-source-dictionary
					   ac-source-words-in-same-mode-buffers
					   ))
	)

  ;; 曖昧マッチ
  (defvar ac-use-fuzzy)
  (setq ac-use-fuzzy t)

  ;; TAB キーで補完を完了する。
  (defvar ac-completing-map)
  (define-key ac-completing-map (kbd "TAB") 'ac-complete)

  ;; 候補が1つしかない時にタブキー押下で補完を完了する。
  ;; 次 / 前候補を選んだ時にタブキー押下で補完を完了する。
  ;; 補完後にメニューを自動的に非表示にする。
  (defvar ac-dwim)
  (setq ac-dwim t)

  ;; ポップアップメニューの自動起動
  (defvar ac-auto-show-menu)
  (setq ac-auto-show-menu nil))

;;------------------------------------------------------------------------------
;; auto-complete-c-headers
;;------------------------------------------------------------------------------
;; C/C++ のヘッダの情報源
;; from package

;; C/C++ モードで自動的に有効化
(defun auto-complete-c-headers-hooks ()
  (add-to-list 'ac-sources 'ac-source-c-headers))

(let ((loadfile "auto-complete-c-headers"))
  (cond ((autoload-if-found 'ac-source-c-headers loadfile t)
		 (dolist (hook '(c-mode-hook c++-mode-hook))
		   (add-hook hook 'auto-complete-c-headers-hooks)))
		(t (display-loading-error-message loadfile))))

