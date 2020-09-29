(use-package multiple-cursors
  :bind
  ("C-x C-@" . mc/edit-lines) ;; Terminal
  ("M-/" . mc/mark-next-like-this)
  ("M-m" . mc/mark-previous-like-this)
  ("C-c M-/" . mc/mark-all-like-this)
  ("M-RET" . set-rectangular-region-anchor)

  :custom
  (mc/edit-lines-empty-lines 'ignore)

  :config
  ;; Make alt-<click> add additional cursors
  (global-unset-key (kbd "M-<down-mouse-1>")) ;; must unset key first
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

;; Allows searching forward/back (C-s/C-r) searching with multiple cursors.
(use-package phi-search
  :custom
  (phi-search-limit 3000))

(provide 'nexus-multiple-cursors)
