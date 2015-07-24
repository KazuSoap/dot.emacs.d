;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; coding system
;;------------------------------------------------------------------------------
;; デフォルトの文字コードを設定
;; 指定される文字コードは以下の項目
;; ① ファイルを新規作成した場合のデフォルト
;; ② サブプロセスでの IO
;; ③ 他の項目が指定されていない場合のデフォルト値
(prefer-coding-system 'utf-8-unix)