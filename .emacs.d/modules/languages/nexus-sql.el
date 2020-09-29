(require 'nexus-rainbow)

(use-package sql-mode
  :straight (:type built-in)
  :mode "\\.sql\\'"
  :hook (sql-mode . nexus-sql-mode-setup)

  :init
  (defun nexus-sql-mode-setup ()
    (setq tab-width 2)

    (company-mode +1)))

(use-package sqlformat
  :hook
  (sql-mode . sqlformat-mode)

  :custom
  (sqlformat-command 'pgformatter))

(provide 'nexus-sql)
