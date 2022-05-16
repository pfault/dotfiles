(defvar nexus-themes-dir (expand-file-name "themes" nexus-dir)
    "Root directory for Emacs custom themes.")
(add-to-list 'custom-theme-load-path nexus-themes-dir)

(provide 'nexus-core-themes)
