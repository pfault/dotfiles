(require 'nexus-workspace-map)

(use-package zoom-window
  :bind
  (:map nexus-workspace-map
        ("RET" . zoom-window-zoom)
        ("C-<return>" . zoom-window-zoom))

  :config
  (with-eval-after-load "persp-mode"
    (setq zoom-window-use-persp t)
    (zoom-window-setup))

  (with-eval-after-load "elscreen"
    (setq zoom-window-use-elscreen t)
    (zoom-window-setup)))

(provide 'nexus-zoom-window)
