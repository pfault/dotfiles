(use-package prescient
  :defer t

  :custom
  (prescient-filter-method '(literal regexp initialism fuzzy))
  (prescient-save-file (expand-file-name "prescient-save.el" nexus-cache-dir)))

(use-package company-prescient
    :after company
    :demand
    :config
    (company-prescient-mode t))

(provide 'nexus-prescient)
