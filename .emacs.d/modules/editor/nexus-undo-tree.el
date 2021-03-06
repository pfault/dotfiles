(use-package undo-tree
  :hook
  (emacs-startup . global-undo-tree-mode)

  :bind
  (:map undo-tree-map
        ("C-x u" . undo-tree-visualize)
        ("M--" . undo-tree-undo)
        ("M-_" . undo-tree-redo)
        ("s-z" . undo-tree-undo)
        ("s-Z" . undo-tree-redo))

  :diminish
  undo-tree-mode

  :custom
  (undo-tree-history-directory-alist
   `((".*" . ,(expand-file-name "undo-tree-history" nexus-cache-dir))))
  (undo-tree-auto-save-history nil) ;; use undohist package instead

  :config
  ;; Unbind keys that I don't use.
  (unbind-key "C-/" undo-tree-map)
  (unbind-key "C-?" undo-tree-map)
  (unbind-key "C-_" undo-tree-map))

(provide 'nexus-undo-tree)
