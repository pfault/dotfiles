;;; package --- nexus-org-re-reveal
;;; Commentary:
;;; org-re-reveal configuration

(require 'nexus-org-mode)

;;; Code:

(use-package org-re-reveal
  :ensure t
  :demand t
  :init
  (setq org-re-reveal-root (concat "file://" (expand-file-name (concat org-directory "reveal.js"))))
  )

(provide 'nexus-org-re-reveal)
;;; nexus-org-re-reveal.el ends here
