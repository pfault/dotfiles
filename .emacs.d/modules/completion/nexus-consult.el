(use-package consult
  :commands (consult-ripgrep)
  :general
  (general-nmap
    :states '(normal insert)
    "C-p" 'consult-yank-pop)
  (pfault/leader-keys
    "s i" '(consult-isearch :wk "isearch")
    "s o" '(consult-outline :which-key "outline")
    "s s" 'consult-line
    "s p" '(consult-ripgrep :wk "ripgrep project")
    "b b" 'consult-buffer
    ;; TODO consult mark
    "f r" 'consult-recent-file
    "s !" '(consult-flymake :wk "flymake"))
  ;; (with-eval-after-load 'projectile
  ;;   (pfault/leader-keys
  ;;     "s p" '((lambda () (interactive) (consult-ripgrep (projectile-project-root))) :wk "ripgrep")))
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  ;; :init
  ;; (setq consult-preview-key "C-l")
  ;; (setq consult-narrow-key ">")
  )

(use-package consult-selectrum
  :after selectrum
  :demand)

(use-package embark-consult
  :straight (embark-consult :type git :host github :repo "oantolin/embark" :files ("embark-consult.el"))
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))
