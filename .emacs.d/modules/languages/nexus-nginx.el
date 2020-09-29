(use-package nginx-mode
  :hook
  (nginx-mode . nexus-nginx-mode-setup)

  :custom
  (nginx-indent-level 4)
  (nginx-indent-tabs-mode nil)

  :init
  (defun nexus-nginx-mode-setup ()
    (setq tab-width 4)))

(use-package company-nginx
  :hook
  (nginx-mode . company-nginx-keywords))

(provide 'nexus-nginx)
