;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; general key bind
;;------------------------------------------------------------------------------
;; kbd -> C-.
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key [backspace] 'backward-delete-char)
(global-set-key (kbd "C-x <pause>") 'save-buffers-kill-emacs);for -nw mode

;;for auto-complete
(global-set-key (kbd "M-<RET>") 'auto-complete-mode)

;; for smart-compile
(global-set-key (kbd "C-c c") 'smart-compile)

;; for helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-z C-e") 'helm-elscreen)

;(global-set-key (kbd "C-c a")   'align)
;(global-set-key (kbd "C-c M-a") 'align-regexp)
;(global-set-key (kbd "C-c d")   'delete-indentation)
;(global-set-key (kbd "M-g")     'goto-line)
;(global-set-key (kbd "C-S-i")   'indent-region)
;(global-set-key (kbd "C-m")     'newline-and-indent)
;(global-set-key (kbd "C-t")     'next-multiframe-window)
;(global-set-key (kbd "M-<RET>") 'ns-toggle-fullscreen)
;(global-set-key (kbd "C-S-t")   'previous-multiframe-window)
;(global-set-key (kbd "C-M-r")   'replace-regexp)
;(global-set-key (kbd "C-r")     'replace-string)

;;------------------------------------------------------------------------------
;; sticky shift
;;------------------------------------------------------------------------------
(defvar sticky-key ";")
(defvar sticky-list
  '(("a" . "A")("b" . "B")("c" . "C")("d" . "D")("e" . "E")("f" . "F")("g" . "G")
	("h" . "H")("i" . "I")("j" . "J")("k" . "K")("l" . "L")("m" . "M")("n" . "N")
	("o" . "O")("p" . "P")("q" . "Q")("r" . "R")("s" . "S")("t" . "T")("u" . "U")
	("v" . "V")("w" . "W")("x" . "X")("y" . "Y")("z" . "Z")
	("1" . "!")("2" . "@")("3" . "#")("4" . "$")("5" . "%")("6" . "^")("7" . "&")
	("8" . "*")("9" . "(")("0" . ")")
	("`" . "~")("[" . "{")("]" . "}")("-" . "_")("=" . "+")("," . "<")("." . ">")
	("/" . "?")(";" . ":")("'" . "\"")("\\" . "|")
	))
(defvar sticky-map (make-sparse-keymap))
(define-key global-map sticky-key sticky-map)
(mapc (lambda (pair)
		  (define-key sticky-map (car pair)
			`(lambda()(interactive)
			   (setq unread-command-events
					 (cons ,(string-to-char (cdr pair)) unread-command-events)))))
		sticky-list)
(define-key sticky-map sticky-key '(lambda ()(interactive)(insert sticky-key)))

