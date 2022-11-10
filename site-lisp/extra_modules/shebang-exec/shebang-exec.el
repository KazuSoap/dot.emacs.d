;;; shebang-exec.el --- shebang for Windows Emacs   -*- lexical-binding: t; -*-

;; Copyright (C) 2018 fubuki

;; Author: fubuki@*****.org
;; Keywords: processes, eshell, shell-command, executable-p

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Sun Feb 11 15:53:03 2018 [Stable version] Eschedule eshell library use version.
;; for (version) "GNU Emacs 25.3.1 (i686-w64-mingw32) of 2017-09-18"
;; for (version) "GNU Emacs 25.2.1 (i686-w64-mingw32) of 2017-04-25"

;; The SHEBANG script of Cygwin can be executed directly from Emacs for Windows.

;;; Installation:

;; (when (eq system-type 'windows-nt)
;;   (require 'shebang-exec))

;;; Code:

(require 'eshell)

(defvar shebang-make-process (fboundp 'make-process))

;; Remove the path part from the executable file in the script.
;; This will cause the execution file to be searched from `exec-path'.
(defvar shebang-file-name-nondirectory t)

(defun shebang-file-name-nondirectory (file)
  (if shebang-file-name-nondirectory
      (ignore-errors (file-name-nondirectory file))
    file))

(defun shebang-truename (file &optional prev-file)
  "Return of cygwin symlink file truename. PREV-FILE is dummy arg."
  (if (null file)
      prev-file
    (shebang-truename (shebang-file-symlink-p file) file)))

(defun shebang-file-symlink-p (file)
  "If FILE is a symlink file of cygwin it returns non NIL."
  (when (file-regular-p file)
    (with-temp-buffer
      (insert-file-contents-literally file nil 0 128)
      (if (looking-at "^!<symlink>\\(.+\\)\0")
          (concat (file-name-directory file) (match-string 1))
        nil))))

(defun shebang-script-interpreter (file)
  (eshell-script-interpreter (eshell-search-path file)))

(defun shebang-which (file)
  (interactive "swhich: ")
  (let* ((file   (eshell-search-path file))
         (script (ignore-errors (eshell-script-interpreter file))))
    (princ (or script file "not found."))))

(defun shebang-call-process (func PROGRAM &optional INFILE DESTINATION DISPLAY &rest ARGS)
  "`call-process' for shebang."
  (let ((shebang (shebang-script-interpreter PROGRAM)))
    (when (consp shebang)
      (setq ARGS (append (cdr shebang) ARGS)
            PROGRAM (car shebang)))
    (apply func PROGRAM INFILE DESTINATION DISPLAY ARGS)))
(advice-add 'call-process :around 'shebang-call-process)

(defun shebang-call-process-region
    (func START END PROGRAM &optional DELETE DESTINATION DISPLAY &rest ARGS)
  "`call-process-region' for shebang."
  (let ((shebang (shebang-script-interpreter PROGRAM)))
    (when (consp shebang)
      (setq ARGS (append (cdr shebang) ARGS)
            PROGRAM (car shebang)))
    (apply func START END PROGRAM DELETE DESTINATION DISPLAY ARGS)))
(advice-add 'call-process-region :around 'shebang-call-process-region)

(cond
 (shebang-make-process
  (defun shebang-make-process (func &rest ARGS)
    "`make-process' for shebang."
    (let* ((save         ARGS)
           (ARGS         (memq :command ARGS))
           (command-args (cadr ARGS))
           (next         (cddr ARGS))
           (shebang      (shebang-script-interpreter (car command-args))))
      (when (consp shebang)
        (setcdr ARGS (cons (append shebang (cdr command-args)) next)))
      (apply func save)))
  (advice-add 'make-process :around 'shebang-make-process))

 (t
  (defun shebang-start-process (func NAME BUFFER-OR-NAME PROGRAM &rest ARGS)
    "`start-process' for shebang."
    (let ((shebang (shebang-script-interpreter PROGRAM)))
      (when (consp shebang)
        (setq ARGS (append (cdr shebang) ARGS)
              PROGRAM (car shebang)))
      (apply func NAME BUFFER-OR-NAME PROGRAM ARGS)))
  (advice-add 'start-process :around 'shebang-start-process)))

(defun shebang-file-executable-p (func file)
  (let ((file (shebang-truename file)))
    (or (funcall func file) (eshell-script-interpreter file))))
(advice-add 'file-executable-p :around 'shebang-file-executable-p)

(defun shebang-eshell-script-interpreter (func file)
  "Retuen value of interpreter argment list or NIL."
  (let ((file (funcall func file)))
    (and file (cons (shebang-file-name-nondirectory (car file)) (cdr file)))))
(advice-add 'eshell-script-interpreter :around 'shebang-eshell-script-interpreter)

(defun shebang-eshell-search-path (func name)
  "Return of full path name in $PATH env."
  (shebang-truename (funcall func name)))
(advice-add 'eshell-search-path :around 'shebang-eshell-search-path)

(defun shebang-exec-disable-advice nil
  "shebang disable advice."
  (interactive)
  (advice-remove 'call-process              'shebang-call-process)
  (advice-remove 'call-process-region       'shebang-call-process-region)
  (advice-remove 'file-executable-p         'shebang-file-executable-p)
  (advice-remove 'eshell-script-interpreter 'shebang-eshell-script-interpreter)
  (advice-remove 'eshell-search-path        'shebang-eshell-search-path)
  (when shebang-make-process
    (advice-remove 'make-process 'shebang-make-process))
  (advice-remove 'start-process 'shebang-start-process))

(provide 'shebang-exec)
;; fine.
