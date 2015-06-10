;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; fakecygpty
;;------------------------------------------------------------------------------
;; process-connection-type が nil で start-process がコールされるけれども、fakecygpty を経由して
;; 起動したいプログラムの名称を列挙する
(defvar fakecygpty-program-list '("mozc_emacs_helper.sh"))

;; process-connection-type が nil 以外で start-process がコールされるけれども、fakecygpty を経由
;; したプロセスを走らせたくないバッファの名称を列挙する
(defvar fakecygpty-exclusion-buffer-name-list '("*grep*"))

;; fakecygpty を経由するかを判断してプログラムを起動する
(defadvice start-process (around ad-start-process-to-fake last activate)
  (when (and (or process-connection-type
				 (member (replace-regexp-in-string "\\.exe$" "" (file-name-nondirectory (ad-get-arg 2)))
						 fakecygpty-program-list))
			 (not (member (if (bufferp (ad-get-arg 1))
							  (buffer-name (ad-get-arg 1))
							(ad-get-arg 1))
						  fakecygpty-exclusion-buffer-name-list)))
	(ad-set-args 3 (cons (ad-get-arg 2) (ad-get-args 3)))
	(ad-set-arg 2 "fakecygpty"))
  ad-do-it)

;; emacs-24.4 では、fakecygpty を経由して起動したプロセスに対し、4096バイトを超える入力を一回に送り込むと
;; レスポンスが帰ってこない状況となる。これを改善する。（NTEmacsスレッド４ 714氏 の投稿を一部修正したもの）
(defconst w32-pipe-limit 4096)

(defadvice process-send-string (around ad-process-send-string activate)
  (if (not (eq (process-type (ad-get-arg 0)) 'real))
      ad-do-it
    (let* ((process (or (ad-get-arg 0)
                        (get-buffer-process (current-buffer))))
           (send-string (encode-coding-string (ad-get-arg 1)
                                              (cdr (process-coding-system (get-process process)))))
           (send-string-length (length send-string)))
      (if (<= send-string-length w32-pipe-limit)
          ad-do-it
        (let ((inhibit-eol-conversion t)
              (from 0)
              (to))
          (while (< from send-string-length)
            (setq to (min (+ from w32-pipe-limit) send-string-length))
            (ad-set-arg 1 (substring send-string from to))
            ad-do-it
            (setq from to)))))))
;;------------------------------------------------------------------------------
