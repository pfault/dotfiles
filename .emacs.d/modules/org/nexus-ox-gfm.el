(require 'nexus-org-mode)

(use-package ox-gfm
  :defer t

  :hook
  (org-mode . nexus-ox-gfm-setup)

  :init
  (defun nexus-ox-gfm-setup ()
    (require 'ox-gfm)))

(provide 'nexus-ox-gfm)
