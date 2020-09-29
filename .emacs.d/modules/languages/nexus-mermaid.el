(require 'nexus-prog-mode)

(use-package mermaid-mode
  :mode "\\.mermaid\\'" "\\.mmd\\'"

  :hook
  (mermaid-mode . nexus-mermaid-mode-setup)

  :init
  (defun nexus-mermaid-mode-setup ()
    (run-hooks 'prog-mode-hook)))

(provide 'nexus-mermaid)
