;;; -*- coding: utf-8; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; cygwin-mount
;; cygwin → win の path 変換
;; from : package system
;;------------------------------------------------------------------------------

(require 'cygwin-mount)
(setq cygwin-mount-program "D:/msys64/usr/bin/mount")
(setq cygwin-mount-uname-program "D:/msys64/usr/bin/uname")
(cygwin-mount-activate)
