(require 'nexus-org-mode)

(use-package ox-pandoc
  :defer t

  :hook
  (org-mode . nexus-ox-pandoc-setup)

  :custom
  (org-pandoc-options-for-gfm '((columns . "80")))
  (org-pandoc-options-for-markdown '((columns . "80")))
  (org-pandoc-options-for-org '((columns . "80")))

  :init
  (defun nexus-ox-pandoc-setup ()
    (require 'ox-pandoc)))

(provide 'nexus-ox-pandoc)
