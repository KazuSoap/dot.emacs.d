;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; twittering-mode
;; twittering-mode.el is a major mode for Twitter.
;; from : package system
;;------------------------------------------------------------------------------

(with-eval-after-load "twittering-mode"
  ;; パスワード暗号ファイル保存先変更(デフォはホームディレクトリ)
  (defvar twittering-private-info-file)
  (setq twittering-private-info-file "~/.emacs.d/twittering-mode/.twittering-mode.gpg")

  ;; 認証済みaccess tokenをGnuPGで暗号化して保存する
  (defvar twittering-use-master-password)
  (setq twittering-use-master-password t)

  ;; sslを使用
  (defvar twittering-use-ssl)
  (setq twittering-use-ssl t)

  ;; 表示する書式
  ;; Items:
  ;; %s - スクリーンネーム
  ;; %S - アカウント名
  ;; %i - プロフィール画像
  ;; %d - description
  ;; %l - 位置情報
  ;; %L - "[位置情報]"
  ;; %r - ダイレクトメッセージ時に"sent to user",リプライ時に"in reply to user"の形式で相手の名前を表示
  ;; %R - RT時にRTしたユーザ名"(retweeted by user)"が表示される
  ;; %RT{...} - RT時に{}内の文章がツイートと置き換えられて表示
  ;; %u - url
  ;; %j - user.id
  ;; %p - 鍵垢なら[x]を表示
  ;; %c - ツイート時間を表示(UTC時間)
  ;; %C{time-format-str} - %cの内容を指定したformatで表示
  ;; %@{time-format-str} - X seconds agoという形式で表示
  ;; %T - ツイート内容
  ;; %t - text filled as one paragraph
  ;; %' - truncated
  ;; %FACE[face-name]{...} - {}内の文字列を加工する
  ;; %FIELD[format-str]{field-name}
  ;; - a value of the given field of a tweet formatted with format-str.
  ;; The format-str is optional. As a field-name, you can use
  ;; "retweet_count", "favorite_count" and so on.
  ;; %FIELD-IF-NONZERO[format-str]{field-name}
  ;; - similar to %FIELD[...]{...} except that this makes an empty string
  ;; if the field value is zero.
  ;; %FILL[prefix]{...} - strings filled as a paragraph. The prefix is optional.
  ;; You can use any other specifiers in braces.
  ;; %FOLD[prefix]{...} - strings folded within the frame width.
  ;; The prefix is optional. This keeps newlines and does not
  ;; squeeze a series of white spaces.
  ;; You can use any other specifiers in braces.
  ;; %f - ツイート元
  ;; %# - id
  ;; default : "%i %s,  %@:\\n%FILL{  %T // from %f%L%r%R}\n \"
  (defvar twittering-status-format)
  (setq twittering-status-format
		"%i %FACE[solid]{%S} %FACE[shadow]{@%s}%p%L %@{  }\n%FILL{%T}\n%FACE[shadow]{[%f]%r %RT{RT by %s}}\n--------------------------------------------------")

  ;; %FILL中の文字幅
  (defvar twittering-fill-column)
  (setq twittering-fill-column 60)

  ;; アイコン表示
  (defvar twittering-icon-mode)
  (setq twittering-icon-mode t)

  ;; アイコン保存
  (defvar twittering-use-icon-storage)
  (setq twittering-use-icon-storage t)

  ;; アイコンの保存場所
  (defvar twittering-icon-storage-file)
  (setq twittering-icon-storage-file "~/.emacs.d/twittering-mode/.twittering-mode-icons.gz")

  ;; アイコンサイズ変更
  ;; (defvar twittering-convert-fix-size)
  ;; (setq twittering-convert-fix-size 50)

  ;; タイムラインを90秒間隔で更新
  (defvar twittering-timer-interval)
  (setq twittering-timer-interval 90)

  ;; アイコン取得時の情報表示をデフォルトで抑制するか
  (defvar twittering-url-show-status)
  (setq twittering-url-show-status t)

  ;; 全てのアイコンを保存するか
  (defvar twittering-icon-storage-limit)
  (setq twittering-icon-storage-limit t)

  ;; 最初に開くタイムラインを設定する
  (defvar twittering-initial-timeline-spec-string)
  (setq twittering-initial-timeline-spec-string
		'(":replies"
		  ":home"
		  ))
  )
