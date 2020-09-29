(require 'nexus-lisp)
(require 'nexus-string-inflection)

(defun nexus-recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (string-prefix-p nexus-dir (file-truename buffer-file-name))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil t))

(defun nexus-conditional-emacs-lisp-checker ()
  "Don't check doc style in Emacs Lisp test files."
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match-p ".*-tests?\\.el\\'" file-name))
      (setq-local flycheck-checkers '(emacs-lisp)))))

(defun nexus-emacs-lisp-mode-setup ()
  "Sensible defaults for `emacs-lisp-mode'."
  ;; (run-hooks 'nexus-lisp-coding-hook)
  ;; (eldoc-mode +1)
  ;; (nexus-recompile-elc-on-save)
  ;; (rainbow-mode +1)
  ;; (setq mode-name "EL")
  ;; (nexus-conditional-emacs-lisp-checker)
  )

(add-hook 'emacs-lisp-mode-hook #'nexus-emacs-lisp-mode-setup)
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
(define-key emacs-lisp-mode-map (kbd "C-c C-u") 'string-inflection-all-cycle)

(provide 'nexus-emacs-lisp)
