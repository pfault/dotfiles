(use-package undo-tree
  :demand
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
  ;; Use undohist package to persist history to disk, it seems more reliable
  ;; than undo-tree's auto-save feature which randomly fails to restore history
  ;; for no obvious reason.
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-diff t)

  :config
  (global-undo-tree-mode)

  ;; Unbind keys that I don't use.
  (unbind-key "C-/" undo-tree-map)
  (unbind-key "C-?" undo-tree-map)
  (unbind-key "C-_" undo-tree-map))

(provide 'nexus-undo-tree)
