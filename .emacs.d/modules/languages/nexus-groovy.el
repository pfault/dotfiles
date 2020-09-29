(use-package groovy-mode
  :mode "\\.groovy\\'"
  :hook
  (groovy-mode . nexus-groovy-mode-setup)

  :init
  (defun nexus-groovy-mode-setup ()
        (setq groovy-highlight-assignments t
	                groovy-indent-offset 4
			          tab-width 4)

	    (subword-mode +1)))

(provide 'nexus-groovy)
