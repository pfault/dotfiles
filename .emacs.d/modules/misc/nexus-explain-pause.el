(use-package explain-pause-mode
  :straight (:type git :host github :repo "lastquestion/explain-pause-mode")

  :custom
  (explain-pause-blocking-too-long-ms 40))

(provide 'nexus-explain-pause)
