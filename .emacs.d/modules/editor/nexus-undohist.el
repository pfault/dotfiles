(use-package undohist
  :demand
  :custom
  (undohist-directory (expand-file-name "undohist" nexus-cache-dir))
  (undohist-ignored-files '("COMMIT_EDITMSG"))

  :config
  (undohist-initialize))

(provide 'nexus-undohist)
