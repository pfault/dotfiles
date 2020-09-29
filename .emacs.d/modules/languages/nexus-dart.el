(require 'nexus-company)
(require 'nexus-folding)
(require 'nexus-lsp)
(require 'nexus-projectile)

(use-package dart-mode
  :mode "\\.dart\\'"
  :interpreter "dart"

  :hook
  (dart-mode . nexus-dart-mode-setup)

  :custom
  (dart-format-on-save t)
  (dart-enable-analysis-server t)

  :init
  (with-eval-after-load "projectile"
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

  (defun nexus-dart-mode-setup ()
    (when (fboundp 'highlight-symbol-mode)
      (highlight-symbol-mode -1))
    (when (fboundp 'auto-highlight-symbol-mode)
      (auto-highlight-symbol-mode -1))

    (company-mode +1)
    (lsp)
    (nexus-folding)
    (subword-mode +1)))

(provide 'nexus-dart)
