(defvar nexus-modules-dir (expand-file-name "modules" nexus-dir)
  "Root directory for Emacs modules.")
(nexus-recursive-add-to-load-path nexus-modules-dir)

;; Core
;;(require 'nexus-aliases)
(require 'nexus-packages)
(require 'nexus-hydra)

;; Theme
(require 'nexus-doom-themes)

;; Completion
(require 'nexus-company)
(require 'nexus-selectrum)

;; Documentation
(require 'nexus-helpful)

;; Editor
(require 'nexus-amx)
(require 'nexus-highlight-symbol)
(require 'nexus-display-fill-column)
(require 'nexus-display-indentation)
(require 'nexus-display-line-numbers)
(require 'nexus-folding)
(require 'nexus-minions)
(require 'nexus-mwim)
(require 'nexus-rainbow)
(require 'nexus-undo-tree)
;;(require 'nexus-undohist)
(require 'nexus-which-key)
(require 'nexus-volatile-highlights)

;; Linting
(require 'nexus-flycheck)

;; Misc.
;;(require 'nexus-explain-pause)
(require 'nexus-grip)
(require 'nexus-restart-emacs)
(require 'nexus-zone)

;; Navigation
(require 'nexus-anzu)
(require 'nexus-avy)
;;(require 'nexus-centaur-tabs)
(require 'nexus-dired)
(require 'nexus-git-link)
(require 'nexus-imenu)
(require 'nexus-pulsar)
;;(require 'nexus-recursive-narrow)
;;(require 'nexus-scroll-half-screen)

;; Project management
(require 'nexus-editorconfig)
(require 'nexus-projectile)
(require 'nexus-treemacs)

;; Language Servers
(require 'nexus-lsp)
(require 'nexus-lsp-ui)

;; Shell
(require 'nexus-shell-pop)

;; Spelling
(require 'nexus-flyspell)

;; Text editing
(require 'nexus-expand-region)
;;(require 'nexus-goto-chg)
(require 'nexus-move-dup)
(require 'nexus-multiple-cursors)
(require 'nexus-randomize-region)
(require 'nexus-safe-change-case)
;;(require 'nexus-smart-shift)
(require 'nexus-smartparens)
;;(require 'nexus-electric-pair)
;;(require 'nexus-sort-symbols)
;;(require 'nexus-sort-words)
;;(require 'nexus-string-edit)
(require 'nexus-string-inflection)
;;(require 'nexus-toggle-comments)
(require 'nexus-toggle-quotes)
(require 'nexus-yasnippet)

;; Version control
(require 'nexus-diff-hl)
(require 'nexus-ediff)
(require 'nexus-forge)
(require 'nexus-git-timemachine)
(require 'nexus-github)
(require 'nexus-magit)

;; Window management
(require 'nexus-resize-window)
(require 'nexus-windmove)
(require 'nexus-zoom-window)

;; Workspace management
;;(require 'nexus-tab-bar)
;;(require 'nexus-desktop)
(require 'nexus-dashboard)

;; Org-mode
(require 'nexus-htmlize)
(require 'nexus-ob-mermaid)
(require 'nexus-org-mode)
(require 'nexus-org-roam)
(require 'nexus-org-re-reveal)
(require 'nexus-org-caldav)
(require 'nexus-ox-gfm)
(require 'nexus-ox-pandoc)

;; Mail
(require 'nexus-mu4e)

;; News
(require 'nexus-elfeed)

;; Tools
(require 'nexus-docker)
(require 'nexus-kubernetes)

;; Major modes
(require 'nexus-fundamental-mode)
(require 'nexus-prog-mode)
(require 'nexus-special-mode)
(require 'nexus-text-mode)

;; Languages
(require 'nexus-coffee)
(require 'nexus-conf)
(require 'nexus-css)
(require 'nexus-cucumber)
(require 'nexus-dart)
(require 'nexus-dockerfile)
(require 'nexus-emacs-lisp)
(require 'nexus-flutter)
(require 'nexus-git-modes)
(require 'nexus-golang)
(require 'nexus-groovy)
(require 'nexus-haml)
(require 'nexus-haskell)
(require 'nexus-jinja2)
(require 'nexus-js)
(require 'nexus-json)
(require 'nexus-jsx)
(require 'nexus-lisp)
(require 'nexus-lua)
(require 'nexus-makefile)
(require 'nexus-markdown)
(require 'nexus-mermaid)
(require 'nexus-nginx)
(require 'nexus-php)
(require 'nexus-plantuml)
(require 'nexus-ruby)
(require 'nexus-rustic)
(require 'nexus-sass)
(require 'nexus-scss)
(require 'nexus-sh)
(require 'nexus-slim)
(require 'nexus-sql)
(require 'nexus-terraform)
(require 'nexus-thrift)
(require 'nexus-toml)
(require 'nexus-typescript)
(require 'nexus-vue)
(require 'nexus-web-mode)
(require 'nexus-xml)
(require 'nexus-yaml)

(provide 'nexus-core-modules)
