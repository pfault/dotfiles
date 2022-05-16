(require 'nexus-undo-tree)

(use-package volatile-highlights
  :demand
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))

(provide 'nexus-volatile-highlights)
