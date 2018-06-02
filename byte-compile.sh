#!/usr/bin/bash

emacs -batch -f batch-byte-compile ./init.el
emacs -batch --eval '(byte-recompile-directory "./site-lisp" 0)'
