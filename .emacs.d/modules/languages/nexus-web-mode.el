(require 'nexus-company)
(require 'nexus-display-fill-column)
(require 'nexus-folding)

(use-package web-mode
  :mode
  "\\.html\\'"
  "\\.html.erb\\'"
  "\\.tpl\\'"

  :bind (:map web-mode-map
              ("C-j" . newline-and-indent)
              ("C-c C-h" . nexus-folding-toggle))

  :hook
  (web-mode . nexus-web-mode-setup)

  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-sql-indent-offset 2)
  (web-mode-engines-alist '(("go" . "\\.tpl\\'")))

  :init
  (defun nexus-web-mode-setup ()
    "Default tweaks for `web-mode'."
    (setq tab-width 2)

    (when (version< emacs-version "27.0")
      (nexus-display-fill-column -1))

    (company-mode +1)
    (nexus-folding)
    (subword-mode +1)))

(use-package web-beautify
  :bind (:map web-mode-map
              ("C-c C-f" . web-beautify-html)))

(provide 'nexus-web-mode)
