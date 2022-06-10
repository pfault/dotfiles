;;; package --- nexus-mu4e
;;; Commentary:
;;; mu4e configuration

;;; Code:

(use-package mu4e
  :ensure nil
  ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")

  ;;(setq mu4e-drafts-folder "/Drafts")
  ;;(setq mu4e-sent-folder   "/Sent")
  ;;(setq mu4e-refile-folder "/[Gmail]/All Mail")
  ;;(setq mu4e-trash-folder  "/Trash")
  (setq mail-user-agent 'mu4e-user-agent)
  (setq message-kill-buffer-on-exit t)

  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "PFault f4n.de"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/pfault-f4n" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-full-name . "Martin Schrodi")
                    (user-mail-address . "pfault@f4n.de")
                    (mu4e-sent-folder . "/pfault-f4n/Sent")
                    (mu4e-trash-folder . "/pfault-f4n/Trash")
                    (mu4e-drafts-folder . "/pfault-f4n/Drafts")
                    (mu4e-refile-folder . "/pfault-f4n/Archive")
                    (mu4e-sent-messages-behavior . sent)
                    ))
          ))
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask-if-none)

  ;; Prevent mu4e from permanently deleting trashed items
  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg)
                            (mu4e-get-trash-folder msg))
              :action (lambda (docid msg target)
                        ;; Here's the main difference to the regular trash mark,
                        ;; no +T before -N so the message is not marked as
                        ;; IMAP-deleted:
                        (mu4e--server-move docid (mu4e--mark-check-target target) "-N"))))

  ;; Display options
  ;;(setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)
  (mu4e t)
  :bind
  ("C-x M" . 'mu4e)
  )

(use-package org-mime
  :config
  (require 'org-mime)
  :bind
  (:map message-mode-map
        ("C-c o m" . org-mime-edit-mail-in-org-mode))
  :hook
  (message-send . org-mime-confirm-when-no-multipart))

(use-package mu4e-alert
  :after mu4e
  :config
  ;; Show unread emails from all inboxes
  ;;(setq mu4e-alert-interesting-mail-query pfault/mu4e-inbox-query)

  ;; Show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil
        )

  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  )

(provide 'nexus-mu4e)
;;; nexus-mu4e.el ends here
