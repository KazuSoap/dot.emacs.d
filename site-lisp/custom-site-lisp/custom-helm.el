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
(helm-mode 1)

;; tramp で remote-directory を開いているときに、helm-for-files を起動すると反応が悪い
;; 原因は helm-source-files-in-current-dir だったので、この情報源の指定を削除する
;; また、一部表示順を変更する
(setq helm-for-files-preferred-list
      '(helm-source-buffers-list
        helm-source-bookmarks
        helm-source-recentf
        helm-source-file-cache
        ;; helm-source-files-in-current-dir
        helm-source-locate))

;; 表示する最大候補数を指定する（デフォルトで 50）
;; (setq helm-candidate-number-limit 100)

;; 候補表示画面で改行しないようにする
;; (setq helm-truncate-lines t)

;; helm-source-buffers-list を詳細に表示しない
(setq helm-buffer-details-flag nil)

;; helm-source-buffers-list でバッファ名を表示する幅を調整する
(setq helm-buffer-max-length 50)

;; helm-swoop で検索結果を cycle しない
;;(setq helm-swoop-move-to-line-cycle nil)

;; helm-follow-mode （C-c C-f で ON/OFF）の前回の状態を維持する
(setq helm-follow-mode-persistent t)

;; ミニバッファで C-k 入力時にカーソル以降を削除する（C-u C-k でも同様の動きをする）
(setq helm-delete-minibuffer-contents-from-point t)

;; http://fukuyama.co/nonexpansion
;; 自動補完を無効にする
(setq helm-ff-auto-update-initial-value nil)

;; C-h でバックスペースと同じように文字を削除できるようにする
;; (define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)

;; TAB で補完する
;;(define-key helm-read-file-map (kbd "<tab>") 'helm-execute-persistent-action)

(require 'helm-config)
;; http://d.hatena.ne.jp/sugyan/20120104/1325604433
;; プレフィックスキーを C-; に設定する
(custom-set-variables '(helm-command-prefix-key "C-;"))

;; キーバインドを設定する。コマンド起動後は、以下のキーが利用可能となる
;;  ・M-n     ：カーソル位置の単語を検索パターンに追加
;;  ・C-z     ：チラ見
;;  ・C-c C-f ：helm-follow-mode の ON/OFF
(global-set-key (kbd "C-x C-b") 'helm-for-files)
(define-key helm-command-map (kbd "C-;") 'helm-resume)
(define-key helm-command-map (kbd "y")   'helm-show-kill-ring)
(define-key helm-command-map (kbd "o")   'helm-occur)
(define-key helm-command-map (kbd "C-s") 'helm-occur-from-isearch)
;;(define-key helm-command-map (kbd "s")   'helm-swoop)
(define-key helm-command-map (kbd "g")   'helm-do-grep) ;;C-u 付で起動すると recursive となる
(define-key helm-command-map (kbd "t")   'helm-gtags-find-tag)

;; 先頭に"*helm"が付いたバッファーは表示しない
(setq helm-boring-buffer-regexp-list '("^*helm"))

;; ハイライト色の変更
(custom-set-faces
 '(helm-match ((t (:inherit match :foreground "#d70035")))))

;;; helm-migemo ----------------------------------------------------------------
;; helmでmigemo検索
;; from : package system
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
								(helm-candidates-in-buffer (helm-get-current-source)))))
					  (volatile) (match identity)))
			source))
