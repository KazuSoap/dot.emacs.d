(require 'kkc)
(require 'popup)

(defvar quail-japanese-use-double-n)
(setq quail-japanese-use-double-n t)

(defvar kkc-show-conversion-list-count)
(setq kkc-show-conversion-list-count 0)

(defvar kkc-popup-min-width 20)
(defvar kkc-popup-width kkc-popup-min-width)
(defvar kkc-cand-popup nil)

(defun ad-kkc-show-conversion-list-update ()
  (or kkc-current-conversions-width
      (kkc-setup-current-conversions-width))
  (let* ((current-idx (car kkc-current-conversions))
         (first-slot (aref kkc-current-conversions-width 0))
         (this-idx (aref first-slot 0))
         (next-idx (aref first-slot 1))
         (msg (aref first-slot 2))
         (max-width kkc-popup-width))
    (if (< current-idx this-idx)
        (setq this-idx 1 msg nil max-width kkc-popup-min-width)
      (if (>= current-idx next-idx)
          (setq this-idx next-idx msg nil max-width kkc-popup-min-width)))
    (if (not msg)
        (let ((len (length kkc-current-conversions))
              (width-table kkc-current-conversions-width)
              (max-items (length kkc-show-conversion-list-index-chars))
              idx l)
          (while (<= (+ this-idx max-items) current-idx)
            (setq this-idx (+ this-idx max-items)))
          (aset first-slot 0 (setq idx this-idx))

          (while (and (< idx len)
                      (< (- idx this-idx) max-items))
            (when (< max-width (aref width-table idx))
              (setq max-width (aref width-table idx)))
            (setq idx (1+ idx)))
          (aset first-slot 1 (setq next-idx idx))
          (setq kkc-popup-width max-width)

          (setq l (nthcdr this-idx kkc-current-conversions))
          (setq idx this-idx)
          (while (< idx next-idx)
            (setq msg (append msg (list (format "%c %s"
                                                (aref kkc-show-conversion-list-index-chars
                                                      (- idx this-idx))
                                                (car l))))
                  idx (1+ idx)
                  l (cdr l)))
          (aset first-slot 2 msg)))

    (let ((old-overlay-head-end (overlay-end kkc-overlay-head))
          (old-overlay-tail-end (overlay-end kkc-overlay-tail)))

      ;; prerare for popup msg
      (popup-delete kkc-cand-popup)
      (setq kkc-cand-popup (popup-create
                            (overlay-start kkc-overlay-head)
                            max-width
                            (length kkc-show-conversion-list-index-chars)
                            :around t
                            :selection-face 'highlight))

      ;; fix kkc-overlay regions
      (move-overlay kkc-overlay-head (overlay-start kkc-overlay-head) old-overlay-head-end)
      (move-overlay kkc-overlay-tail (overlay-start kkc-overlay-tail) old-overlay-tail-end))

    (popup-set-list kkc-cand-popup msg)
    (if (> current-idx 0)
        (popup-select kkc-cand-popup (- current-idx this-idx))
      (popup-delete kkc-cand-popup))))

(defun kkc-cand-popup-draw ()
  (unwind-protect
      (when (popup-live-p kkc-cand-popup)
        ;; (> (popup-width my-cand-popup) 0)                   ; not to be corrupted
        ;; (when (and (not (eq width (popup-width my-cand-popup))) ; truncated
        ;;            (not truncate))
        ;;   ;; Refill once again to lines be fitted to popup width
        ;;   (setq width (popup-width my-cand-popup))
        ;;   (setq lines (cdr (popup-fill-string string width width))))
        (popup-draw kkc-cand-popup)

        (clear-this-command-keys)
        (push (read-event nil) unread-command-events))
    (popup-delete kkc-cand-popup)))

(advice-add 'kkc-show-conversion-list-update :override 'ad-kkc-show-conversion-list-update)
(advice-add 'kkc-next :after 'kkc-cand-popup-draw)
(advice-add 'kkc-prev :after 'kkc-cand-popup-draw)
(advice-add 'kkc-show-conversion-list-or-next-group :after 'kkc-cand-popup-draw)
(advice-add 'kkc-show-conversion-list-or-prev-group :after 'kkc-cand-popup-draw)
(advice-add 'kkc-select-from-list :after (lambda() (when kkc-converting (kkc-cand-popup-draw))))
