;;; my-package.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; built-in packages not loaded immediately
;;==============================================================================
;;------------------------------------------------------------------------------
;; display-line-numbers-mode
;;------------------------------------------------------------------------------
(with-eval-after-load 'display-line-numbers
  (set-face-attribute 'line-number nil :background "gray10")
  (set-face-attribute 'line-number-current-line nil :background "gray40"))

;;------------------------------------------------------------------------------
;; ediff
;;------------------------------------------------------------------------------
(eval-when-compile (require 'ediff))

(with-eval-after-load 'ediff
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)

  (let ((bgEdiff "gray20"))
    (set-face-attribute 'ediff-even-diff-A nil :background bgEdiff)
    (set-face-attribute 'ediff-even-diff-B nil :background bgEdiff)
    (set-face-attribute 'ediff-odd-diff-A  nil :background bgEdiff)
    (set-face-attribute 'ediff-odd-diff-B  nil :background bgEdiff)))

;;------------------------------------------------------------------------------
;; TRAMP(Transparent Remote Access Multiple Protocol)
;; edit remoto file from local emacs
;;------------------------------------------------------------------------------
(eval-when-compile (require 'tramp))

(with-eval-after-load 'tramp
  (declare-function tramp-change-syntax "tramp")
  (tramp-change-syntax 'simplified) ; Emacs 26.1 or later
  (setq tramp-encoding-shell "bash")

  ;; When connecting to a remote shell, disable the LC_ALL setting to prevent garbled Japanese characters
  ;; http://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-processes.html#Running%20a%20debugger%20on%20a%20remote%20host
  (let ((process-environment (default-value 'tramp-remote-process-environment)))
    (setenv "LC_ALL" nil)
    (setq tramp-remote-process-environment process-environment)))

;;------------------------------------------------------------------------------
;; whitespace-mode
;; Visualize invisible characters
;;------------------------------------------------------------------------------
(eval-when-compile (require 'whitespace))

(with-eval-after-load 'whitespace
  ;; delete trailing whitespace when saving
  (add-hook 'before-save-hook #'delete-trailing-whitespace)

  ;; list of invisible characters to visualize
  (setq whitespace-style '(face trailing tabs spaces newline space-mark tab-mark newline-mark))

  ;; display mapping of invisible characters
  (setq whitespace-display-mappings
        '(;; hard space > "¤"
          (space-mark   ?\u00A0 [?\u00A4])
          ;; full-width-space > "□"
          (space-mark   ?\u3000 [?\u25a1])
          ;; tab > "»" with underline
          ;; (tab-mark     ?\t     [?\u00BB ?\t])
          ;; newline > "｣"
          (newline-mark ?\n     [?\uFF63 ?\n])))

  ;; recognize "space" if it matches the following regular expression
  (setq whitespace-space-regexp "\\(\u3000+\\)")

  ;; face
  (set-face-attribute 'whitespace-space    nil :foreground "GreenYellow")
  (set-face-attribute 'whitespace-tab      nil :foreground "LightSkyBlue" :underline t)
  (set-face-attribute 'whitespace-newline  nil :foreground "DeepSkyBlue")
  (set-face-attribute 'whitespace-trailing nil :background "DeepPink"))

;;==============================================================================
;; from package
;;==============================================================================
;;------------------------------------------------------------------------------
;; cygwin-mount
;; teach EMACS about cygwin styles and mount points
;; https://www.emacswiki.org/emacs/cygwin-mount.el
;;------------------------------------------------------------------------------
(eval-when-compile
  (when (eq system-type 'windows-nt)
    (require 'cygwin-mount)
    (cygwin-mount-build-table-internal)
    (cygwin-mount-activate))

  (defmacro cygwin-mount-nt ()
    (when (and (eq system-type 'windows-nt)
               (boundp 'cygwin-mount-table--internal))
      (declare-function cygwin-mount-activate "cygwin-mount")

      `(progn
         (with-eval-after-load 'shell
           (require 'cygwin-mount)
           (setq cygwin-mount-table ',cygwin-mount-table--internal)
           (fset 'cygwin-mount-get-cygdrive-prefix (lambda () ,(cygwin-mount-get-cygdrive-prefix)))

           (cygwin-mount-activate))
         ))))
(cygwin-mount-nt)

;;------------------------------------------------------------------------------
;; fakecygpty
;; cygwin's pty feature for NTEmacs
;; https://github.com/d5884/fakecygpty
;;------------------------------------------------------------------------------
(eval-when-compile
  (when (eq system-type 'windows-nt)
    (require 'fakecygpty))

  (defmacro fakecygpty-nt ()
    (when (eq system-type 'windows-nt)
      (declare-function fakecygpty-activate "fakecygpty")

      (let* ((program-path-regexps (concat "^" (expand-file-name "/") "\\(usr/\\(local/\\)?\\)?bin/.*"))
             (ad-fakecygpty--ignored-program
              (lambda (program)
                (not (string-match-p program-path-regexps (executable-find (file-name-nondirectory program)))))))
        `(progn
           (with-eval-after-load 'shell
             (require 'fakecygpty)
             (advice-add 'fakecygpty--ignored-program :before-until ,ad-fakecygpty--ignored-program)

             (fakecygpty-activate))
           )))))
(fakecygpty-nt)

;;------------------------------------------------------------------------------
;; shebang-exec
;; https://qiita.com/s-fubuki/items/67ddcf36970cd5492d2c
;; https://gist.github.com/s-fubuki/819a7f7e7cb2cbcaa5b9bd04dcb1e713
;;------------------------------------------------------------------------------
;; (eval-when-compile
;;   (defmacro shebang-exec-nt ()
;;     (when (eq system-type 'windows-nt)
;;       `(progn
;;          (add-hook 'after-init-hook (lambda () (require 'shebang-exec)))

;;          (with-eval-after-load 'shebang-exec
;;            (fset 'ad-executable-find
;;                  (lambda (&rest args)
;;                    (locate-file (car args) exec-path nil 'file-executable-p)
;;                    ))
;;            (advice-add 'executable-find :after-until 'ad-executable-find))
;;          ))))
;; (shebang-exec-nt)

;;------------------------------------------------------------------------------
;; IME
;;------------------------------------------------------------------------------
(eval-when-compile
  (defmacro ime-settings ()
    (cond ((eq system-type 'windows-nt) ; if
           (require 'tr-ime)
           (require 'w32-ime)
           (declare-function tr-ime-font-reflect-frame-parameter "tr-ime-font")

           `(progn
              (with-eval-after-load 'tr-ime-font
                (setq default-input-method "W32-IME")

                ;; Display Windows IME status in mode line (ON:[あ], OFF:[Aa]).
                (setq w32-ime-mode-line-state-indicator "[Aa]")
                (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

                (w32-ime-initialize)

                (add-hook 'w32-ime-on-hook (apply-partially #'set-cursor-color "yellow"))
                (add-hook 'w32-ime-off-hook (apply-partially #'set-cursor-color "thistle"))

                ;; IME control (Turn off IME when typing "yes" or "no", etc.)
                (w32-ime-wrap-function-to-control-ime #'universal-argument)
                (w32-ime-wrap-function-to-control-ime #'read-string)
                (w32-ime-wrap-function-to-control-ime #'read-char)
                (w32-ime-wrap-function-to-control-ime #'read-from-minibuffer)
                (w32-ime-wrap-function-to-control-ime #'y-or-n-p)
                (w32-ime-wrap-function-to-control-ime #'yes-or-no-p)
                (w32-ime-wrap-function-to-control-ime #'map-y-or-n-p)

                ;; frame font
                (modify-all-frames-parameters `((ime-font . ,(frame-parameter nil 'font))))
                (tr-ime-font-reflect-frame-parameter))
              ))

          ((eq system-type 'gnu/linux) ; else if
           (require 'mozc)

           (let ((;; Intercept the key event of the [muhenkan] key
                  ad-mozc-intercept-keys
                  (lambda (f event)
                    (cond
                     ((member event '(muhenkan))
                      ;; (message "%s" event) ;debug
                      (mozc-clean-up-changes-on-buffer)
                      (mozc-fall-back-on-default-binding event))
                     (t ; else
                      ;; (message "%s" event) ;debug
                      (funcall f event))))))
             `(progn
                (with-eval-after-load 'mozc
                  ;; (setq mozc-candidate-style 'overlay)
                  ;; (require 'mozc-popup)
                  ;; (setq mozc-candidate-style 'popup)
                  (require 'mozc-cand-posframe)
                  (setq mozc-candidate-style 'posframe)

                  (advice-add 'mozc-handle-event :around ,ad-mozc-intercept-keys)
                  (add-hook 'input-method-activate-hook (apply-partially #'set-cursor-color "yellow"))
                  (add-hook 'input-method-deactivate-hook (apply-partially #'set-cursor-color "thistle")))
                )))
          )))
(ime-settings)

;;------------------------------------------------------------------------------
;; company
;; a modular completion framework
;;------------------------------------------------------------------------------
;; company-box
;; a company front-end with icons
;;------------------------------------------------------------------------------
(eval-when-compile (require 'company))

(with-eval-after-load 'company
  (setq company-idle-delay nil)
  (setq company-minimum-prefix-length 3) ; minimum prefix length for idle completion
  (setq company-selection-wrap-around t)
  (define-key (default-value 'company-mode-map) (kbd "C-<tab>") 'company-complete)
  (add-hook 'company-mode-hook 'company-box-mode))

(eval-when-compile
  (require 'company-box)
  (require 'frame-local)
  (declare-function company-box--get-frame "company-box")
  (declare-function frame-local-get "frame-local")

  (defmacro fix-company-box (get-frame-func &rest margs)
    `(lambda (&rest args)
       (let ((frame (,get-frame-func ,(car margs))))
         (when frame
           (set-frame-parameter frame 'tab-bar-lines 0)))
       args)))

(with-eval-after-load 'company-box
  (set-face-attribute 'company-tooltip-selection nil :foreground "wheat" :background "steelblue")
  (set-face-attribute 'company-tooltip nil :background "midnight blue")

  (advice-add 'company-box--display :after (fix-company-box company-box--get-frame))
  (advice-add 'company-box-doc--show :after (fix-company-box frame-local-getq company-box-doc-frame)))

;;------------------------------------------------------------------------------
;; flycheck
;; on-the-fly syntax checking
;;------------------------------------------------------------------------------
(eval-when-compile (require 'flycheck))

(with-eval-after-load 'flycheck
  (setq flycheck-display-errors-delay 0.3)
  ;; (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ; remove useless warnings
  (setq flycheck-emacs-lisp-load-path 'inherit) ; use the `load-path' of the current Emacs session
  (add-hook 'flycheck-mode-hook (lambda () (setq left-fringe-width 8)))) ; Enable left-fringe only in flycheck-mode

;;------------------------------------------------------------------------------
;; helm
;; an Emacs incremental and narrowing framework
;;------------------------------------------------------------------------------
(eval-when-compile (require 'helm-files))

(with-eval-after-load 'helm
  (setq helm-ff-auto-update-initial-value nil)) ; Disable autocomplete

;;------------------------------------------------------------------------------
;; highlight-indent-guides
;; a minor mode highlights indentation levels via font-lock
;;------------------------------------------------------------------------------
(eval-when-compile (require 'highlight-indent-guides))

(with-eval-after-load 'highlight-indent-guides
  ;; (setq-default highlight-indent-guides-method 'character)
  ;; (setq-default highlight-indent-guides-auto-character-face-perc 100)
  ;; (setq-default highlight-indent-guides-character ?\|)
  ;; (setq-default highlight-indent-guides-method 'column)
  (setq highlight-indent-guides-auto-odd-face-perc 40)
  (setq highlight-indent-guides-auto-even-face-perc 25))

;;------------------------------------------------------------------------------
;; irony
;; a C/C++ minor mode powered by libclang
;;------------------------------------------------------------------------------
(eval-when-compile
  (require 'irony)

  (defmacro irony-nt ()
    (when (eq system-type 'windows-nt)
      (let ((;; fixed issue with irony-server-install
             ad-irony--install-server-read-command
             (lambda (args)
               "modify irony--install-server-read-command"
               (setenv "CC" "clang") (setenv "CXX" "clang++") ; set clang to compiler
               `(,(replace-regexp-in-string
                   (format "^\\(%s\\)" (shell-quote-argument (default-value 'irony-cmake-executable)))
                   "\\1 -G'MSYS Makefiles'"
                   (car args))))))
        `(progn
         (with-eval-after-load 'irony
           ;; Set the buffer size to 64K on Windows (from the original 4K)
           (setq irony-server-w32-pipe-buffer-size (* 64 1024))

           (advice-add 'irony--install-server-read-command :filter-args ,ad-irony--install-server-read-command)
           (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))
         )))))
(irony-nt)

;;------------------------------------------------------------------------------
;; magit
;; a Git porcelain inside Emacs
;;------------------------------------------------------------------------------
;; (eval-when-compile
;;   (defmacro magit-nt ()
;;     (when (and (eq system-type 'windows-nt) (fboundp 'start-my-shell-process))
;;       `(progn
;;          (with-eval-after-load 'magit-section
;;            (start-my-shell-process))
;;          ))))
;; (magit-nt)

(eval-when-compile
  (require 'magit)

  (defmacro my-magit ()
    (when (boundp 'git-commit-summary-max-length)
      `(progn
         (with-eval-after-load 'magit
           (setq git-commit-summary-max-length 999)
           (add-hook 'git-commit-setup-hook #'turn-off-auto-fill t))
         ))))
(my-magit)


;;------------------------------------------------------------------------------
;; migemo
;; Japanese incremental search through dynamic pattern expansion
;;------------------------------------------------------------------------------
(let ((ad-migemo-register-isearch-keybinding
       (lambda ()
         (define-key isearch-mode-map (kbd "C-M-y") 'migemo-isearch-yank-char)
         (define-key isearch-mode-map (kbd "C-w") 'migemo-isearch-yank-word)
         (define-key isearch-mode-map (kbd "M-s C-e") 'migemo-isearch-yank-line)
         (define-key isearch-mode-map (kbd "M-m") 'migemo-isearch-toggle-migemo)
         (define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill))))
  (with-eval-after-load 'migemo
    (advice-add 'migemo-register-isearch-keybinding :override ad-migemo-register-isearch-keybinding)))

;;------------------------------------------------------------------------------
;; smert compile
;;------------------------------------------------------------------------------
(eval-when-compile (require 'smart-compile))

(with-eval-after-load 'smart-compile
  (let ((cmd (lambda ()
               (emacs-lisp-byte-compile)
               (native-compile buffer-file-name))))
    (add-to-list 'smart-compile-alist `(emacs-lisp-mode ,cmd))))
(global-set-key (kbd "C-c c") #'smart-compile)

;;==============================================================================
;; major-mode
;;==============================================================================
;;------------------------------------------------------------------------------
;; markdown-mode
;;------------------------------------------------------------------------------
(eval-when-compile (require 'markdown-mode))

(with-eval-after-load 'markdown-mode
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-content-type "application/xhtml+xml")
  (setq markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"))
  (setq markdown-xhtml-header-content "
<style>
body {
  box-sizing: border-box;
  width: 100%;
  margin: 40px auto;
  padding: 0 10px;
}
</style>
<script>
document.addEventListener('DOMContentLoaded', () => {
  document.body.classList.add('markdown-body');
});
</script>
" ))

;;------------------------------------------------------------------------------
;; plantuml-mode
;;------------------------------------------------------------------------------
(eval-when-compile (require 'plantuml-mode))

(with-eval-after-load 'plantuml-mode
  (setq plantuml-jar-path (eval-when-compile (locate-file "plantuml" exec-path '(".jar"))))

  ;; specify the desired output type to use for generated diagrams(svg,png,txt)
  (setq plantuml-output-type "png")
  (setq plantuml-default-exec-mode 'jar))

;;------------------------------------------------------------------------------
;; web-mode
;;------------------------------------------------------------------------------
(eval-when-compile (require 'web-mode))

(with-eval-after-load 'web-mode
  (setq web-mode-engines-alist '(("php" . "\\.phtml\\'")))
  (setq web-mode-content-types-alist '(("js" . "\\.\\(js[x]\\|vue\\)?\\'")))
  (setq web-mode-enable-current-element-highlight t)
  (add-to-list 'web-mode-comment-formats '("javascript" . "//" )))

(provide 'my-package)
;;; my-package.el ends here
