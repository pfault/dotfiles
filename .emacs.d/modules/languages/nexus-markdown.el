(require 'nexus-display-fill-column)
(require 'nexus-display-line-numbers)
(require 'nexus-flyspell)
(require 'nexus-move-dup)
(require 'nexus-polymode)
(require 'nexus-prettier-js)
(require 'nexus-smartparens)

(use-package markdown-mode
  :mode
  "\\.md"
  "\\.mkd"
  "\\.mkdn"
  "\\.mdown"
  "\\.markdown"

  :bind
  (:map markdown-mode-map
        ("C-c p" . markdown-preview)
        ("M-p" . md-move-lines-up)
        ("M-n" . md-move-lines-down)
        ("M-P" . markdown-previous-link)
        ("M-N" . markdown-next-link))

  :hook
  (markdown-mode . nexus-markdown-mode-setup)

  :custom
  (markdown-command "pandoc -f gfm -t html5")

  :custom-face
  (markdown-code-face ((t nil)))

  :init
  (defun nexus-markdown-mode-setup ()
    (setq-local markdown-asymmetric-header t
                prettier-js-args '("--print-width" "80"
                                   "--prose-wrap" "always")
                whitespace-action nil)

    (nexus-display-fill-column)
    (nexus-display-line-numbers)
    (auto-fill-mode)
    (prettier-js-mode)
    (flyspell-mode)
    (smartparens-mode +1)
    (subword-mode)))

;; Required by markdown-edit-code-block.
(use-package edit-indirect
  :defer t)

(provide 'nexus-markdown)
