(require 'nexus-helm)

(use-package helm-ag
  :after (helm-global-bindings)
  :bind
  ("C-x '" . helm-do-ag-project-root)
  ("C-x C-'" . helm-do-ag-project-root)
  (:map helm-command-map
        ("a" . helm-do-ag))

  :custom
  (helm-ag-base-command "rg --no-heading")
  (helm-ag-use-emacs-lisp-regexp nil)
  (helm-ag-ignore-patterns '("*.min-latest.css"
                             "*.min-latest.js"
                             "*.min.css"
                             "*.min.js"
                             "*.sql"
                             "*.test"
                             "archive-contents"
                             "cache"
                             "elpa"
                             "node_modules"
                             "sorbet"
                             "straight"
                             "vendor/assets")))

(provide 'nexus-helm-ag)
