(use-package recursive-narrow
  :bind
  ("C-x C-n" . recursive-narrow-or-widen-dwim)
  ("C-x n w" . recursive-widen)
  ("C-x n n" . recursive-narrow-or-widen-dwim)

  :config
  (add-hook 'recursive-narrow-dwim-functions
            'nexus-recursive-narrow-org-edit-src-code)
  (add-hook 'recursive-narrow-dwim-functions
            'nexus-recursive-narrow-markdown-edit-code-block)

  :init
  (defun nexus-recursive-narrow-org-edit-src-code()
    (ignore-errors (org-edit-src-code) t))

  (defun nexus-recursive-narrow-markdown-edit-code-block()
    (ignore-errors (markdown-edit-code-block) t)))

(provide 'nexus-recursive-narrow)
