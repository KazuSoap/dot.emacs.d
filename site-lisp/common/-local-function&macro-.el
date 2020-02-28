;;------------------------------------------------------------------------------
;; local functions
;;------------------------------------------------------------------------------
;;; my-message-startup-time ----------------------------------------------------
;; 起動時間を [ms] 単位で表示
;; emacs-init-time() は 秒単位で小数点第一位まで
;; http://memo.sugyan.com/entry/20120120/1327037494
(fset 'my-message-startup-time
      (lambda ()
        "echo bootup time in message buffer"
        (message "Emacs loaded in %d ms"
                 (* 1000 (float-time (time-subtract after-init-time before-init-time))))))
(add-hook 'after-init-hook 'my-message-startup-time)

;;; my-revert-buffer-no-confirm ------------------------------------------------
;; バッファを再読み込み
;; https://www.emacswiki.org/emacs/RevertBuffer
(fset 'my-revert-buffer-no-confirm
      (lambda (&optional force-reverting)
        (interactive "P")
        (if (or force-reverting (not (buffer-modified-p)))
            (revert-buffer :ignore-auto :noconfirm)
          (error "The buffer has been modified"))))
(global-set-key (kbd "<f5>") 'my-revert-buffer-no-confirm)

;;; my-window-resizer ----------------------------------------------------------
;; window size を調整
;; http://d.hatena.ne.jp/khiker/20100119/window_resize
;; (fset 'my-window-resizer
;;       (lambda ()
;;         "Control window size and position."
;;         (interactive)
;;         (let ((dx (if (= (nth 0 (window-edges)) 0) 1 -1))
;;               (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
;;               action
;;               c)
;;           (catch 'end-flag
;;             (while t
;;               (setq action (read-key-sequence-vector (format "size[%dx%d]" (window-width) (window-height))))
;;               (setq c (aref action 0))
;;               (cond ((= c ?l) (enlarge-window dx t))
;;                     ((= c ?h) (shrink-window dx t))
;;                     ((= c ?j) (enlarge-window dy))
;;                     ((= c ?k) (shrink-window dy))
;;                     (t (let ((command (key-binding action)))
;;                          (when command (call-interactively command)))
;;                        (message "Quit")
;;                        (throw 'end-flag t))))))))
;; (global-set-key (kbd "C-c C-r") 'my-window-resizer)

;;; my-mark-eob ----------------------------------------------------------------
;; バッファの最後に "[EOB]" を表示
;; http://www.emacswiki.org/cgi-bin/wiki?HighlightEndOfBuffer
(fset 'my-mark-eob
      (lambda ()
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
          (overlay-put eob-mark 'after-string eob-text))))
(add-hook 'find-file-hook 'my-mark-eob)

;;------------------------------------------------------------------------------
;; local macros
;;------------------------------------------------------------------------------
;;; my-debug-message -----------------------------------------------------------
;; 特定の関数を実行された際、引数と出力、backtrace を出力
;; (eval-when-compile
;;   (defmacro my-debug-message (fun-name)
;;     `(progn
;;        (fset (quote ,(intern (format "add-%s" fun-name)))
;;              (lambda (f &rest args)
;;                (backtrace)
;;                ;; (message "(%s)before:%s" ,(format "%s" fun-name) (prin1-to-string args))
;;                (message "(%s)before:%s" ,(format "%s" fun-name) args)

;;                (let ((ret (apply f args)))
;;                    (message "(%s)after:%s" ,(format "%s" fun-name) ret)
;;                  ret)))
;;        (advice-add ',fun-name :around ',(intern (format "add-%s" fun-name)) '((depth . 100))))))
