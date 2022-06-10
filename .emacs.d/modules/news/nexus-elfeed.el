;;; package --- nexus-elfeed.el
;;; Commentary:
;;; elfeed configuration

;;; Code:

(use-package elfeed
  :hook
  (emacs-startup-hook 'elfeed-update)
  :config
  (setq elfeed-feeds (quote
                      (("https://www.uninformativ.de/blog/feeds/en.atom" blog tech)
                       ("https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-and-content.xml" blog tech)
                       ("https://nullprogram.com/feed/" blog tech)
                       ("https://protesilaos.com/master.xml" blog tech emacs)
                       ("https://karthinks.com/index.xml" blog tech emacs)
                       ("http://nulldot.net/index.rss" blog tech emacs)
                       ("https://drewdevault.com/blog/index.xml" blog tech)
                       ("https://xeiaso.net/blog.rss" blog tech)
                       ("https://fasterthanli.me/index.xml" blog tech)
                       ("https://www.lpalmieri.com/rss.xml" blog tech rust)
                       ("https://www.christopherbiscardi.com/rss.xml" blog tech rust)
                       ("https://joelhooks.com/rss.xml" blog tech)
                       ("https://blog.rust-lang.org/feed.xml" blog tech rust)
                       ("https://blog.rust-lang.org/inside-rust/feed.xml" blog tech rust)
                       ("https://lobste.rs/rss" lobsters)
                       ("https://news.ycombinator.com/rss" hackernews)
                       ;;("https://hnrss.org/newest" hackernews)
                       ("https://matrix.org/blog/feed" opsec matrix)
                       ("https://briarproject.org/news/index.xml" opsec)
                       )))
  (elfeed-update)
  :bind
  ("C-x R" . elfeed)
  )

;; (use-package elfeed-goodies
;;   :config
;;   (elfeed-goodies/setup)
;;   (setq elfeed-goodies/entry-pane-size 0.5)
;;   (setq elfeed-goodies/switch-to-entry nil))

(provide 'nexus-elfeed)
;;; nexus-elfeed.el ends here
