;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; fakecygpty
;; NTEmacs の仮想端末偽装
;; https://github.com/trueroad/fakecygpty
;;------------------------------------------------------------------------------

;; process-connection-type が nil で start-process がコールされるけれども、
;; fakecygpty を経由して起動したいプログラムの名称を列挙
(defvar fakecygpty-program-list)
(setq fakecygpty-program-list '(""))

;; fakecygpty を経由するかを判断してプログラムを起動する
(defun ad-start-process-to-fake (orig-fun &rest args)
  (when (and (nth 2 args)
			 (or process-connection-type
				 (member (replace-regexp-in-string "\\.exe$" "" (file-name-nondirectory (nth 2 args)))
						 fakecygpty-program-list)))
	(push "fakecygpty" (nthcdr 2 args)))
  (apply orig-fun args))
(advice-add 'start-process :around 'ad-start-process-to-fake '((depth . 100)))

(defmacro fakecygpty-set-signal (fun-name send-key)
  `(progn
	 (defun ,(intern (format "ad-%s" fun-name)) (orig-fun &rest args)
	   (let ((process (or (car args)
						  (get-buffer-process (current-buffer)))))
		 (if (string= (car (process-command process)) "fakecygpty")
			 (process-send-string (car args) (kbd ,send-key))
		   (apply orig-fun args))))
	 (advice-add (quote ,fun-name) :around (quote ,(intern (format "ad-%s" fun-name))))))

(fakecygpty-set-signal interrupt-process "C-c")
(fakecygpty-set-signal stop-process "C-z")
(fakecygpty-set-signal quit-process "C-\\")
(fakecygpty-set-signal process-send-eof "C-d")

;; emacs-24.4、emacs-24.5 では、4096バイトを超えるデータを一度にパイプ経由で
;; プロセスに送り込むと、レスポンスが帰ってこない状況となる。これを改善する。
;; (NTEmacsスレッド４ 714 の投稿を一部修正したもの。NTEmacsスレッド４ 734、737 の
;; 対策も兼ねるため、 4096バイトを超えない入力の場合でも一律同じ処理を実行している。)
(defconst w32-pipe-limit 4096)

(defun ad-process-send-string (orig-fun &rest args)
  (if (not (eq (process-type (car args)) 'real))
	  (apply orig-fun args)
	(let* ((process (or (car args)
						(get-buffer-process (current-buffer))))
		   (send-string (encode-coding-string (nth 1 args)
											  (cdr (process-coding-system (get-process process)))))
		   (send-string-length (length send-string)))
	  (let ((inhibit-eol-conversion t)
			(from 0)
			to)
		(while (< from send-string-length)
		  (setq to (min (+ from w32-pipe-limit) send-string-length))
		  (setf (nth 1 args) (substring send-string from to))
		  (apply orig-fun args)
		  (setq from to))))))
(advice-add 'process-send-string :around 'ad-process-send-string)
