(require 'nexus-flycheck)
(require 'nexus-folding)
(require 'nexus-js)

(use-package json-mode
  :mode "\\.json\\'"

  :bind (:map json-mode-map
              ("C-j" . newline-and-indent)
              ("C-c C-h" . nexus-folding-toggle))

  :hook
  (json-mode . nexus-json-mode-setup)

  :init
  (defun nexus-json-mode-setup ()
    "Default tweaks for `json-mode'."

    (let ((width 2))
      (setq js-indent-level width
            json-reformat:indent-width width
            tab-width width))

    (setq flycheck-checker 'json-jsonlint)))

(provide 'nexus-json)
