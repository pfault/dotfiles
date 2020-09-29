(use-package dashboard
  :diminish
  (dashboard-mode page-break-lines-mode)
  :custom
  (dashboard-center-content t)
  (dashboard-startup-banner 1)
  (dashboard-items '((recents . 15)
                     (projects . 5)
                     (bookmarks . 5)))
  :custom-face
  (dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
  :hook
  (after-init . dashboard-setup-startup-hook))

(provide 'nexus-dashboard)
