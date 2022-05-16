(use-package amx
  :bind
  ("M-x" . amx)
  ("C-x C-m" . amx)

  :custom
  (amx-backend 'selectrum)
  (amx-histroy-lenth 15)
  (amx-prompt-string "M-x ")
  (amx-save-file (expand-file-name "amx-items" nexus-cache-dir))
  (amx-show-key-bindings t)

  :config
  (amx-mode +1))

(provide 'nexus-amx)
