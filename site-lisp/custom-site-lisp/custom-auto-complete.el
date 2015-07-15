;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; auto-complete
;;------------------------------------------------------------------------------
;;; auto-complete --------------------------------------------------------------
;; 補完システム
;; from package

;; 特定のモードで自動的に有効化
(dolist (hook '(emacs-lisp-mode-hook makefile-mode-hook
				c-mode-hook c++-mode-hook))
  (add-hook hook '(lambda () (auto-complete-mode t))))

(with-eval-after-load 'auto-complete
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
