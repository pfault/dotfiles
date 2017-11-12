(use-package notmuch)
(setq notmuch-search-oldest-first nil
      message-sendmail-envelope-from 'header
      mail-specify-envelope-from 'header
      mail-envelope-from 'header
      notmuch-show-all-multipart/alternative-parts nil
      mime-edit-pgp-signers '("722E6E3E")
      mime-edit-pgp-encrypt-to-self t
      mml2015-encrypt-to-self t
      mml2015-sign-with-sender t
      notmuch-crypto-process-mime t
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "~/bin/msmtp-enqueue.sh"
      message-sendmail-f-is-evil nil
      mail-interactive t
      user-full-name "User Name"
      user-mail-address "account@serv.tld"
      message-kill-buffer-on-exit t
      mail-user-agent 'message-user-agent
      notmuch-always-prompt-for-sender t
      notmuch-fcc-dirs '((".*" . "accountname/Sent"))
      notmuch-show-indent-messages-width 4
      notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                               (:name "unread" :query "tag:unread" :key "u")
                               (:name "flagged" :query "tag:flagged" :key "f")
                               (:name "sent" :query "tag:sent" :key "t")
                               (:name "drafts" :query "tag:draft" :key "d")
                               (:name "all mail" :query "*" :key "a")))

(require 'org-notmuch)

(require 'notmuch-address)
(setq notmuch-address-command "/usr/bin/notmuch-addrlookup")
(notmuch-address-message-insinuate)

(setq notmuch-address-selection-function
  (lambda (prompt collection initial-input)
    (completing-read prompt (cons initial-input collection) nil t nil 'notmuch-address-history)))

(define-key notmuch-show-mode-map "\C-c\C-o" 'browse-url-at-point)

(define-key notmuch-search-mode-map "g"
  'notmuch-poll-and-refresh-this-buffer)
(define-key notmuch-hello-mode-map "g"
  'notmuch-poll-and-refresh-this-buffer)

(define-key notmuch-search-mode-map "d"
  (lambda ()
    "toggle deleted tag for thread"
    (interactive)
    (if (member "deleted" (notmuch-search-get-tags))
        (notmuch-search-tag '("-deleted"))
      (notmuch-search-tag '("+deleted" "-inbox" "-unread")))))

(define-key notmuch-search-mode-map "!"
  (lambda ()
    "toggle unread tag for thread"
    (interactive)
    (if (member "unread" (notmuch-search-get-tags))
        (notmuch-search-tag '("-unread"))
      (notmuch-search-tag '("+unread")))))


(define-key notmuch-show-mode-map "d"
  (lambda ()
    "toggle deleted tag for message"
    (interactive)
    (if (member "deleted" (notmuch-show-get-tags))
        (notmuch-show-tag '("-deleted"))
      (notmuch-show-tag '("+deleted" "-inbox" "-unread")))))


(define-key notmuch-search-mode-map "a"
  (lambda ()
    "toggle archive"
    (interactive)
    (if (member "archive" (notmuch-search-get-tags))
        (notmuch-search-tag '("-archive"))
      (notmuch-search-tag '("+archive" "-inbox" "-unread")))))


(define-key notmuch-show-mode-map "a"
  (lambda ()
    "toggle archive"
    (interactive)
    (if (member "archive" (notmuch-show-get-tags))
        (notmuch-show-tag '("-archive"))
      (notmuch-show-tag '("+archive" "-inbox" "-unread")))))


(define-key notmuch-hello-mode-map "i"
  (lambda ()
    (interactive)
    (notmuch-hello-search "tag:inbox")))

(define-key notmuch-hello-mode-map "u"
  (lambda ()
    (interactive)
    (notmuch-hello-search "tag:unread")))

(define-key notmuch-hello-mode-map "a"
  (lambda ()
    (interactive)
    (notmuch-hello-search "tag:archive")))

(use-package calfw)
(use-package calfw-org)
(use-package org-gcal)
(setq org-gcal-client-id "idhere.apps.googleusercontent.com"
      org-gcal-client-secret "secrethere"
      org-gcal-file-alist '(("account@serv.tld" .  "~/git/org/accountname/schedule.org")
                          ))

(provide 'setup-notmuch)
