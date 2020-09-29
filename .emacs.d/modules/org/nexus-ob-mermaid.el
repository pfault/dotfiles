(require 'nexus-org-mode)

(use-package ob-mermaid
  :defer t

  :hook
  (org-mode . nexus-ob-mermaid-setup)

  :init
  (defun nexus-ob-mermaid-setup ()
    (require 'ob-mermaid)))

(provide 'nexus-ob-mermaid)
