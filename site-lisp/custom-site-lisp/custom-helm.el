;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; helm 設定
;;------------------------------------------------------------------------------
;; helm ------------------------------------------------------------------------
;; emacsに統一的なある”操作方式”を提供するフレームワーク
;; ある情報(ファイル名，バッファ名など)に対して以下の3段階の操作方式を提供
;; 1. インクリメンタルなパターンマッチによる絞り込み
;; 2. 対象の情報の選択
;; 3. 対象の情報に対するアクション(ファイルを開く，削除する)
;; このフレームワーク上で作られた機能は全て同じ操作方式で機能を利用できる
;; 絞り込み -> 選択 -> アクション の操作性を様々な機能に提供するのがhelmの本質
;; from : package system
(with-eval-after-load 'helm
  (helm-mode 1)

  ;; tramp で remote-directory を開いているときに、helm-for-files を起動すると反応が悪い
  ;; 原因は helm-source-files-in-current-dir だったので、この情報源の指定を削除する
  ;; また、一部表示順を変更する
  (defvar helm-for-files-preferred-list)
  (setq helm-for-files-preferred-list
		'(helm-source-buffers-list
		  helm-source-bookmarks
		  helm-source-recentf
		  helm-source-file-cache
		  ;; helm-source-files-in-current-dir
		  helm-source-locate))

  ;; helm-source-buffers-list を詳細に表示しない
  (defvar helm-buffer-details-flag)
  (setq helm-buffer-details-flag nil)

  ;; helm-source-buffers-list でバッファ名を表示する幅を調整する
  (defvar helm-buffer-max-length)
  (setq helm-buffer-max-length 50)

  ;; helm-follow-mode （C-c C-f で ON/OFF）の前回の状態を維持する
  (defvar helm-follow-mode-persistent)
  (setq helm-follow-mode-persistent t)

  ;; ミニバッファで C-k 入力時にカーソル以降を削除する（C-u C-k でも同様の動きをする）
  (defvar helm-delete-minibuffer-contents-from-point)
  (setq helm-delete-minibuffer-contents-from-point t)

  ;; http://fukuyama.co/nonexpansion
  ;; 自動補完を無効にする
  (defvar helm-ff-auto-update-initial-value)
  (setq helm-ff-auto-update-initial-value nil)

  ;; 先頭に"*helm"が付いたバッファーは表示しない
  (defvar helm-boring-buffer-regexp-list)
  (setq helm-boring-buffer-regexp-list '("^*helm"))

  ;; ハイライト色の変更
  (custom-set-faces
   '(helm-match ((t (:inherit match :foreground "#d70035")))))
)
;;; helm-migemo ----------------------------------------------------------------
;; helmでmigemo検索
;; from : package system

(with-eval-after-load 'helm-lib
  (require 'helm-migemo)

  ;; helm で正しく migemo を動作させるための対策
  ;; http://rubikitch.com/2014/12/19/helm-migemo/
  ;; https://github.com/emacs-helm/helm/pull/379
  (defun helm-compile-source--candidates-in-buffer (source)
	(helm-aif (assoc 'candidates-in-buffer source)
		(append source
				`((candidates
				   . ,(or (cdr it)
						  (lambda ()
							;; Do not use `source' because other plugins
							;; (such as helm-migemo) may change it
							(helm-candidates-in-buffer (helm-get-current-source))))
				   )
				  (volatile) (match identity)
				  )
				)source)
	)
  )
