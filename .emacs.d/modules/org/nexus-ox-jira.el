re 'nexus-org-mode)

(use-package ox-jira
  :defer t

  :hook
  (org-mode . nexus-ox-jira-setup)

  :init
  (defun nexus-ox-jira-setup ()
    (require 'ox-jira)))

(provide 'nexus-ox-jira)
