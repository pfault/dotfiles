(require 'nexus-helm)

(use-package helm-open-github
  :after (helm-global-bindings)
  :bind
  ("C-c o f" . helm-open-github-from-file)
  ("C-c o c" . helm-open-github-from-commit)
  ("C-c o i" . helm-open-github-from-issues)
  ("C-c o p" . helm-open-github-from-pull-requests)

  :custom
  (helm-open-github-commit-limit 10000))

(provide 'nexus-helm-open-github)
