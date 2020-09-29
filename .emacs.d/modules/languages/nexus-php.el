(require 'nexus-company)
(require 'nexus-folding)
(require 'nexus-prettier-js)
(require 'nexus-rainbow)

(use-package php-mode
  :interpreter "php"
  :mode "\\.php\\'" "\\.inc\\'" "\\.module\\'"
  :hook
  (php-mode . nexus-php-mode-setup)

  :init
  (defun nexus-php-mode-setup ()
    (prettier-js-mode)
    (rainbow-mode +1)
    (company-mode +1)
    (subword-mode +1)
    (nexus-folding)))

(provide 'nexus-php)
