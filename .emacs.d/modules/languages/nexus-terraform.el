(require 'nexus-company)
(require 'nexus-reformatter)

(use-package terraform-mode
  :hook
  (terraform-mode . nexus-terraform-mode-setup)

  :custom
  (terraform-indent-level 2)

  :init
  (defun nexus-terraform-mode-setup ()
    (setq tab-width 2)
    (terraform-format-on-save-mode 1))

  :config
  ;; This does a better job of injecting formatted content than the default
  ;; formatting commands included with terraform-mode.
  (reformatter-define terraform-format
    :program "terraform"
    :args '("fmt" "-no-color" "-")
    :lighter " TF"))

(use-package company-terraform
  :init
  (with-eval-after-load 'terraform-mode
    (company-terraform-init)))

(use-package terraform-doc
  :defer t)

(provide 'nexus-terraform)
