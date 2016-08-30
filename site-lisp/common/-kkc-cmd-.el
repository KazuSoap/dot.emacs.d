;; kkc-cmd.el --- KKC with external command

(require 'kkc)
(eval-when-compile (require 'kkc))

(defvar kkc-lookup-command "TSFTest.exe")
(defvar kkc-lookup-command-conding-system 'cp932)

(defvar kkc-lookup-command-cache nil
  "kkc-run-lookup-commandの出力をキャッシュする。
部分変換される長いキーの場合は、変換候補リストの代わりに部分キーを格納する。")

(defvar kkc-overlay-tail-original "")
(defvar kkc-overlay-tail-converted "")

(defvar kkc-selecting-learned nil)
(defvar kkc-changing-length nil)

(setq kkc-lookup-cache (list (list))) ; kkc.el が使う

(defun kkc-run-lookup-command-body (key)
  "kkc-run-lookup-command を実行して KEY を変換し、リストで結果を返す。
CAR が実際に変換した文字列、CDR が変換候補。"
  (let (start entry conversions converted)
    (with-temp-buffer
      (setq-local coding-system-for-read kkc-lookup-command-conding-system)
      (call-process kkc-lookup-command nil t nil
                    (encode-coding-string key
                                          kkc-lookup-command-conding-system))
      (goto-char (point-min))
      (while (setq start (search-forward " : " nil t))
        (setq entry (buffer-substring-no-properties start (line-end-position)))
        (unless (member entry conversions)
          (setq conversions (append conversions (list entry)))
          (if (and (null converted)
                   (string-prefix-p entry key))
              (setq converted entry)))))
    (when (null converted)
      (setq conversions (list (concat "外部コマンド("
                                      kkc-lookup-command
                                      ")が期待する動作をしていません")))
      (setq converted key))
    (cons converted conversions)))

(defun kkc-run-lookup-command (key)
  "kkc-lookup-command とキャッシュを使って KEY を変換し、リストで結果を返す。
結果は4要素で、KEYの変換した部分、変換候補のリスト、
最後に選択した候補の番号、未変換部分の文節リスト。"
  (unless (hash-table-p kkc-lookup-command-cache)
    (setq kkc-lookup-command-cache (make-hash-table :test 'equal)))
  (let ((ret (gethash key kkc-lookup-command-cache))
        conversions converted)
    (unless ret
      (let ((out (kkc-run-lookup-command-body key)))
        (setq converted (car out))
        (setq conversions (cdr out)))
      (if (equal converted key)
          (setq ret (list converted conversions 1 nil))
        (let ((rest (kkc-run-lookup-command (substring key (length converted))))
              (cached (gethash converted kkc-lookup-command-cache))
              (idx 1))
          (if cached
              (setq idx (nth 2 cached))
            (puthash converted (list converted conversions 1 nil)
                     kkc-lookup-command-cache))
          (setq ret (list converted converted idx
                          (cons (car rest) (nth 3 rest))))))
      (puthash key ret kkc-lookup-command-cache))
    (while (stringp (cadr ret))
      (let ((cached (gethash (cadr ret) kkc-lookup-command-cache)))
        (setq ret (list (car ret) (cadr cached) (nth 2 cached) (nth 3 ret)))))
    ret))

(defun kkc-lookup-keylist (list)
  "キーのLISTをキャッシュを参照して変換する"
  (mapconcat (function (lambda (x)
                         (let ((cached (gethash x kkc-lookup-command-cache)))
                           (nth (- (nth 2 cached) 1) (cadr cached)))))
             list ""))

(defun kkc-lookup-key-with-command (len &optional postfix prefer-noun)
  "LEN を変換して候補リスト(kkc-current-conversions)を返す。
副作用として kkc-current-conversions, kkc-current-conversions-width,
kkc-length-head, kkc-length-converted, kkc-selecting-learned をセットする。
POSTFIX と PREFER-NOUN はオリジナルと引数を合わせるためのダミー"
  (let* ((key (substring kkc-original-kana 0 len))
         (command-output (kkc-run-lookup-command key))
         (converted (car command-output))
         (conversions (cadr command-output))
         (selected (nth 2 command-output))
         (rest (nth 3 command-output)))
    (if kkc-changing-length
        (let ((extend (substring kkc-original-kana (length converted) len)))
          (if (< len (length kkc-original-kana))
              (let ((follow (kkc-run-lookup-command
                             (substring kkc-original-kana len))))
                (if (= len (length converted))
                    (puthash kkc-original-kana
                             (list converted conversions selected
                                   (cons (car follow) (nth 3 follow)))
                             kkc-lookup-command-cache)
                  (remhash kkc-original-kana kkc-lookup-command-cache)
                  (kkc-run-lookup-command kkc-original-kana))
                (setq kkc-overlay-tail-converted
                      (concat extend
                              (nth (- (nth 2 follow) 1) (cadr follow))
                              (kkc-lookup-keylist (nth 3 follow)))))
            (puthash kkc-original-kana
                     (list converted converted selected (list extend))
                     kkc-lookup-command-cache)
            (setq kkc-overlay-tail-converted extend))
          (setq kkc-length-head len)
          (setq kkc-changing-length nil))
      (setq kkc-overlay-tail-converted (kkc-lookup-keylist rest))
      (setq kkc-length-head (length converted)))
    (setq kkc-selecting-learned t)
    (setq kkc-length-converted (length converted))
    (setq kkc-current-conversions-width nil)
    (setq kkc-current-conversions (cons selected conversions))))

(unless (fboundp 'kkc-lookup-key-original)
  (fset 'kkc-lookup-key-original (symbol-function 'kkc-lookup-key)))
(defalias 'kkc-lookup-key 'kkc-lookup-key-with-command)

(defun kkc-overlay-tail-convert ()
  (let ((head-end (overlay-end kkc-overlay-head))
        (tail-start (overlay-start kkc-overlay-tail))
        (tail-end (overlay-end kkc-overlay-tail)))
    (if (= tail-start tail-end)
        (setq kkc-overlay-tail-original "")
      (goto-char tail-end)
      (let ((converted kkc-overlay-tail-converted)
            (overlap (- kkc-length-head kkc-length-converted)))
        (when (and (= (car kkc-current-conversions) -1)
                   (> overlap 0))
          (setq converted (substring kkc-overlay-tail-converted 0 overlap))
          (setq converted (concat (japanese-katakana converted)
                                  (substring kkc-overlay-tail-converted
                                             overlap))))
        (insert converted))
      (setq kkc-overlay-tail-original (buffer-substring  tail-start tail-end))
      (delete-region tail-start tail-end)
      (if (< tail-start head-end)
          (move-overlay kkc-overlay-head
                        (overlay-start kkc-overlay-head) head-end))
      (goto-char (overlay-end kkc-overlay-tail)))))

(defun kkc-overlay-tail-unconvert ()
  (let ((head-end (overlay-end kkc-overlay-head))
        (tail-start (overlay-start kkc-overlay-tail))
        (tail-end (overlay-end kkc-overlay-tail)))
    (unless (= tail-start tail-end)
      (goto-char tail-end)
      (insert kkc-overlay-tail-original)
      (delete-region tail-start tail-end)
      (if (< tail-start head-end)
          (move-overlay kkc-overlay-head
                        (overlay-start kkc-overlay-head) head-end))
      (goto-char (overlay-end kkc-overlay-tail)))))

(unless (fboundp 'kkc-next-original)
  (fset 'kkc-next-original (symbol-function 'kkc-next)))
(defun kkc-next-plus ()
  "Select the next candidate of conversion."
  (interactive)
  (when kkc-selecting-learned
    (unless (= (car kkc-current-conversions) 1)
      (setcar kkc-current-conversions 0))
    (setq kkc-selecting-learned nil))
  (kkc-next-original))
(defalias 'kkc-next 'kkc-next-plus)

(unless (fboundp 'kkc-prev-original)
  (fset 'kkc-prev-original (symbol-function 'kkc-prev)))
(defun kkc-prev-plus ()
  "Select the previous candidate of conversion."
  (interactive)
  (when kkc-selecting-learned
    (setcar kkc-current-conversions 1)
    (setq kkc-selecting-learned nil))
  (kkc-prev-original))
(defalias 'kkc-prev 'kkc-prev-plus)

(unless (fboundp 'kkc-shorter-original)
  (fset 'kkc-shorter-original (symbol-function 'kkc-shorter)))
(defun kkc-shorter-plus ()
  "Make the Kana string to be converted shorter."
  (interactive)
  (if (<= kkc-length-head 1)
      (kkc-error "Can't be shorter"))
  (kkc-overlay-tail-unconvert)
  (setq kkc-length-head (1- kkc-length-head))
  (setq kkc-changing-length t)
  (kkc-lookup-key-with-command kkc-length-head)
  (kkc-update-conversion 'all))
(defalias 'kkc-shorter 'kkc-shorter-plus)

(unless (fboundp 'kkc-shorter-conversion-original)
  (fset 'kkc-shorter-conversion-original
        (symbol-function 'kkc-shorter-conversion)))
(defalias 'kkc-shorter-conversion 'kkc-shorter)

(unless (fboundp 'kkc-longer-original)
  (fset 'kkc-longer-original (symbol-function 'kkc-longer)))
(defun kkc-longer-plus ()
  (interactive)
  (if (>= kkc-length-head (length kkc-current-key))
      (kkc-error "Can't be longer"))
  (kkc-overlay-tail-unconvert)
  (setq kkc-length-head (1+ kkc-length-head))
  (setq kkc-changing-length t)
  (kkc-lookup-key-with-command kkc-length-head)
  (kkc-update-conversion 'all))
(defalias 'kkc-longer 'kkc-longer-plus)

(unless (fboundp 'kkc-longer-phrase-original)
  (fset 'kkc-longer-phrase-original (symbol-function 'kkc-longer-phrase)))
(defalias 'kkc-longer-phrase 'kkc-longer)

(unless (fboundp 'kkc-next-phrase-original)
  (fset 'kkc-next-phrase-original (symbol-function 'kkc-next-phrase)))
(defun kkc-next-phrase-plus ()
  (interactive)
  (if (>= kkc-length-head (length kkc-original-kana))
      (kkc-terminate)
    (setq kkc-original-kana (substring kkc-original-kana kkc-length-head))
    (setq kkc-current-key (string-to-vector kkc-original-kana))
    (goto-char (overlay-end kkc-overlay-head))
    (delete-region (point) (overlay-end kkc-overlay-tail))
    (move-overlay kkc-overlay-head (point) (point))
    (insert kkc-original-kana)
    (move-overlay kkc-overlay-tail (point) (point))
    (setq kkc-length-head (length kkc-original-kana))
    (setq kkc-length-converted 0)
    (kkc-lookup-key (length kkc-original-kana))
    (kkc-update-conversion 'all)))
(defalias 'kkc-next-phrase 'kkc-next-phrase-plus)

(unless (fboundp 'kkc-update-conversion-original)
  (fset 'kkc-update-conversion-original
        (symbol-function 'kkc-update-conversion)))
(defun kkc-update-conversion-plus (&optional all)
  (kkc-update-conversion-original 'all)	; 'all を指定して全部書き換える
  (kkc-overlay-tail-convert)
  (let ((entry (gethash (substring kkc-original-kana 0 kkc-length-converted)
                        kkc-lookup-command-cache)))
    (if (>= (car kkc-current-conversions) 1)
        (setcar (cddr entry) (car kkc-current-conversions))
      (let ((key (buffer-substring (overlay-start kkc-overlay-head)
                                   (overlay-end kkc-overlay-head)))
            (idx 1))
        (while (not (equal key (nth idx kkc-current-conversions)))
          (setq idx (+ idx 1))
          (when (>= idx (length kkc-current-conversions)) ; 見つからない場合
            (setq idx (car kkc-current-conversions))
            (setq key (nth idx kkc-current-conversions))))
        (setcar (cddr entry) idx)))))
(defalias 'kkc-update-conversion 'kkc-update-conversion-plus)

;; Local Variables:
;; coding: utf-8-dos
;; End:
