(use-package yasnippet
  :diminish yas-minor-mode
  :hook
  (prog-mode . yas-minor-mode)

  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :defer t
  :after (yasnippet))

(provide 'nexus-yasnippet)
