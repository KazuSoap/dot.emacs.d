;;------------------------------------------------------------------------------
;; local functions & macro
;;------------------------------------------------------------------------------
(defun message-startup-time ()
  "echo bootup time in message buffer"
  (message "Emacs loaded in %d ms"
           (* 1000 (float-time (time-subtract after-init-time before-init-time)))))
(add-hook 'after-init-hook 'message-startup-time)

(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))

(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((dx (if (= (nth 0 (window-edges)) 0) 1 -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
        action
        c)
    (catch 'end-flag
      (while t
        (setq action (read-key-sequence-vector (format "size[%dx%d]" (window-width) (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l) (enlarge-window dx t))
              ((= c ?h) (shrink-window dx t))
              ((= c ?j) (enlarge-window dy))
              ((= c ?k) (shrink-window dy))
              (t (let ((command (key-binding action)))
                   (when command (call-interactively command)))
                 (message "Quit")
                 (throw 'end-flag t)))))))
