(unless (fboundp 'comment-or-uncomment-region-or-line)
  (nexus-allow-line-as-region-for-function comment-or-uncomment-region))

(global-set-key (kbd "C-c /") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c C-_") 'comment-or-uncomment-region-or-line)

(provide 'nexus-toggle-comments)
