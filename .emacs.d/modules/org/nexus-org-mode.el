;;; package --- nexus-org-mode
;;; Commentary:
;;; org-mode configuration

;;; Code:

(require 'nexus-display-fill-column)
(require 'nexus-display-indentation)
(require 'nexus-display-line-numbers)
(require 'nexus-flyspell)
(require 'nexus-smartparens)
(require 'nexus-global-keybindings)

(use-package org
  :straight org-plus-contrib
  :bind
  (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-x C-l" . org-store-link)
         ;; custom functions
         ("M-o a" . org-agenda)
         ("M-o M-o" . bh/show-org-agenda)
         ("M-o d" . bh/org-todo)
         ("M-o h" . bh/hide-other)
         ("M-o c" . org-cycle-agenda-files)
         ("M-o c" . calendar)
         ("M-o I" . bh/punch-in)
         ("M-o O" . bh/punch-out)
         ("M-o SPC" . bh/clock-in-last-task)
         ("M-o t l" . org-toggle-link-display)
         ("M-o t i" . org-toggle-inline-images)
         ("M-o t n" . bh/toggle-next-task-display)
         ("M-o o o" . bh/make-org-scratch)
         ("M-o o p" . (lambda () (interactive) (pfault/open-org-file "~/git/org/refile.org")))
         ("M-o o y" . (lambda () (interactive) (pfault/open-org-file (pfault/get-yesterday-diary))))
         ("M-o o c" . (lambda () (interactive) (pfault/open-org-file (pfault/get-diary-from-cal))))
         ("M-o o t" . (lambda () (interactive) (pfault/open-org-file (pfault/get-today-diary))))
         :map org-mode-map
         ("C-c i" . org-clock-in)
         ("C-c o" . org-clock-out)
         ("C-c n" . org-narrow-to-subtree)
         ("C-c b" . org-narrow-to-block)
         ("C-c w" . bh/widen)
         ("C-c e" . org-set-effort)
         ("C-c >" . org-time-stamp-inactive)
         ("C-c l" . hydra-org-clock/body))
  (:map org-mode-map
        ("C-j" . org-return-indent)
        ("RET" . org-return-indent)
        ("M-{" . org-promote-subtree)
        ("M-}" . org-demote-subtree)
        ("M-P" . org-metaup)
        ("M-N" . org-metadown)
        ("C-M-n" . outline-next-visible-heading)
        ("C-M-p" . outline-previous-visible-heading))
  (:map org-src-mode-map
        ("C-c C-c" . org-edit-src-exit))

  :hook
  (kill-emacs . pfault/org-clock-out-and-save-when-exit)
  (org-mode . nexus-org-mode-setup)

  :custom
  (org-blank-before-new-entry '((heading . auto) (plain-list-item . nil)))
  (org-catch-invisible-edits 'show)
  (org-ctrl-k-protect-subtree t)
  (org-export-backends '(ascii html icalendar latex md))
  (org-export-with-section-numbers nil)
  (org-export-with-sub-superscripts '{})
  (org-return-follows-link t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)

  (org-directory (if (file-directory-p "~/git/org")
                     "~/git/org" "~/org"))
  (org-default-notes-file "~/git/org/refile.org")
  (org-todo-keywords
    (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
           (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
  (org-todo-keyword-faces
   (quote (("TODO" :foreground "red" :weight bold)
           ("MEXT" :foreground "blue" :weight bold)
           ("DONE" :foreground "forest green" :weight bold)
           ("WAITING" :foreground "orange" :weight bold)
           ("HOLD" :foreground "magenta" :weight bold)
           ("CANCELLED" :foreground "forest green" :weight bold)
           ("MEETING" :foreground "forest green" :weight bold)
           ("PHONE" :foreground "forest green" :weight bold))))
  (org-todo-state-tags-triggers
   (quote (("CANCELLED" ("CANCELLED" . t))
           ("WAITING" ("WAITING" . t))
           ("HOLD" ("WAITING") ("HOLD" . t))
           (done ("WAITING") ("HOLD"))
           ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
           ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
           ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
  (org-use-fast-todo-selection t)
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  (org-tag-alist (quote ((:startgroup)
                         ("@errand" . ?e)
                         ("@office" . ?o)
                         ("@home" . ?H)
                         ("@garden" . ?g)
                         (:endgroup)
                         ("WAITING" . ?w)
                         ("HOLD" . ?h)
                         ("PERSONAL" . ?P)
                         ("WORK" . ?W)
                         ("CLAUDI" . ?C)
                         ("ZOOPLUS" . ?Z)
                         ("GARDEN" . ?G)
                         ("ORG" . ?O)
                         ("F4N" . ?N)
                         ("crypt" . ?E)
                         ("NOTE" . ?n)
                         ("CANCELLED" . ?c)
                         ("FLAGGED" . ??))))
  (org-fast-tag-selection-single-key (quote expert))
  (org-global-properties (quote (("Effort_ALL" . "00:05 00:10 00:15 00:30 01:00 01:30 02:00 02:30 03:00")
                                 ("STYLE_ALL" . "habit"))))
  (org-agenda-files (quote ("~/git/org"
                            "~/git/org/pfault"
                            "~/git/org/zooplus")))
  (org-capture-templates
      (quote (("t" "todo" entry (file+headline "~/git/org/refile.org" "Tasks")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/git/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-
finish t)
              ("n" "note" entry (file+headline "~/git/org/refile.org" "Notes")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+olp+datetree "~/git/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file+headline "~/git/org/refile.org" "Review")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/git/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("c" "Phone call" entry (file "~/git/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/git/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE:
 habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
              ("p" "Protocol" entry (file+headline "~/git/org/refile.org" "Review")
               "* TODO Review %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
              ("L" "Protocol Link" entry (file+headline "~/git/org/refile.org" "Review")
               "* TODO Review %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
              )))
  (org-modules (quote (org-bbdb org-checklist org-crypt org-id org-info org-habit org-inlinetask org-protocol)))
  (org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
  (org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
                             org-clone-delete-id t)
  (org-habit-graph-column 50)
  (org-archive-mark-done nil)
  (org-archive-location "%s_archive::* Archived Tasks")
  (org-alphabetical-lists t)
  (org-stuck-projects (quote ("" nil nil "")))
  (org-show-entry-below (quote ((default))))
  (org-time-stamp-rounding-minutes (quote (1 1)))
  (org-enforce-todo-dependencies t)
  (org-hide-leading-stars nil)
  (org-startup-indented t)
  (org-cycle-separator-lines 0)
  (org-blank-before-new-entry (quote ((heading)
                                      (plain-list-item . auto))))
  (org-insert-heading-respect-content nil)
  (org-reverse-note-order nil)
  (org-show-following-heading t)
  (org-show-hierarchy-above t)
  (org-show-siblings (quote ((default))))
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-yank-adjusted-subtrees t)
  (org-id-method (quote uuidgen))
  (org-deadline-warning-days 30)
  (org-table-export-default-format "orgtbl-to-csv")
  (org-tags-exclude-from-inheritance (quote ("crypt")))
  (org-crypt-key "5E05F53FB06BFA30")
  (org-crypt-disable-auto-save t)
  (org-use-speed-commands t)
  (org-export-with-timestamps nil)
  (org-return-follows-link t)
  (org-remove-highlights-with-change t)
  (org-read-date-prefer-future 'time)
  (org-tags-match-list-sublevels t)
  (org-table-use-standard-references (quote from))
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-catch-invisible-edits 'error)
  (org-export-coding-system 'utf-8)
  (org-cycle-include-plain-lists t)
  (org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
  (org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                              ("/" italic "<i>" "</i>")
                              ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                              ("=" org-code "<code>" "</code>" verbatim)
                              ("~" org-verbatim "<code>" "</code>" verbatim))))
  (org-structure-template-alist
    (quote (("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
            ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
            ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
            ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
            ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
            ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
            ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
            ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
            ("H" "#+html: " "<literal style=\"html\">?</literal>")
            ("a" "#+begin_ascii\n?\n#+end_ascii")
            ("A" "#+ascii: ")
            ("i" "#+index: ?" "#+index: ?")
            ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))))
  (org-file-apps (quote ((auto-mode . emacs)
                         ("\\.mm\\'" . system)
                         ("\\.x?html?\\'" . system)
                         ("\\.pdf\\'" . system))))
  (org-speed-commands-user (quote (("0" . ignore)
                                      ("1" . ignore)
                                      ("2" . ignore)
                                      ("3" . ignore)
                                      ("4" . ignore)
                                      ("5" . ignore)
                                      ("6" . ignore)
                                      ("7" . ignore)
                                      ("8" . ignore)
                                      ("9" . ignore)

                                      ("a" . ignore)
                                      ("d" . ignore)
                                      ("h" . bh/hide-other)
                                      ("i" progn
                                       (forward-char 1)
                                       (call-interactively 'org-insert-heading-respect-content))
                                      ("k" . org-kill-note-or-show-branches)
                                      ("l" . ignore)
                                      ("m" . ignore)
                                      ("q" . bh/show-org-agenda)
                                      ("r" . ignore)
                                      ("s" . org-save-all-org-buffers)
                                      ("w" . org-refile)
                                      ("x" . ignore)
                                      ("y" . ignore)
                                      ("z" . org-add-note)

                                      ("A" . ignore)
                                      ("B" . ignore)
                                      ("E" . ignore)
                                      ("F" . bh/restrict-to-file-or-follow)
                                      ("G" . ignore)
                                      ("H" . ignore)
                                      ("J" . org-clock-goto)
                                      ("K" . ignore)
                                      ("L" . ignore)
                                      ("M" . ignore)
                                      ("N" . bh/narrow-to-org-subtree)
                                      ("P" . bh/narrow-to-org-project)
                                      ("Q" . ignore)
                                      ("R" . ignore)
                                      ("S" . ignore)
                                      ("T" . bh/org-todo)
                                      ("U" . bh/narrow-up-one-org-level)
                                      ("V" . ignore)
                                      ("W" . bh/widen)
                                      ("X" . ignore)
                                      ("Y" . ignore)
                                      ("Z" . ignore))))
  ;; startup
  (org-confirm-babel-evaluate nil)
  (org-babel-results-keyword "results")
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  ;; clocking
  (org-log-done t)
  (org-clock-idle-time nil)
  (org-clock-continuously nil)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-persist t)
  (org-clock-report-include-clocking-task t)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  (org-clock-into-drawer t)
  (org-drawers (quote ("PROPERTIES" "LOGBOOK")))
  (org-clock-in-switch-to-state 'bh/clock-in-to-next)
  (bh/keep-clock-running nil)
  (org-clock-in-resume t)
  (org-clock-out-when-done t)
  (org-clock-history-length 23)
  (org-clock-persist-query-resume nil)
  (org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  (org-expiry-inactive-timestamps t)
  ;; refile config
  (org-refile-targets (quote ((nil :maxlevel . 9)
                              (org-agenda-files :maxlevel . 9))))
  (org-refile-use-outline-path t)
  (org-refile-allow-creating-parent-nodes (quote confirm))
  (org-outline-path-complete-in-steps nil)
  (org-refile-target-verify-function 'bh/verify-refile-target)
  ;; agenda
  (org-columns-default-format "%50ITEM(Task) %5TODO(Todo) %10Effort(Effort){:} %10CLOCKSUM(Clock) %2PRIORITY %TAGS")
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-span 'day)
  (org-agenda-show-future-repeats nil)
  (org-agenda-start-on-weekday t)
  (org-agenda-persistent-filter t)
  (org-agenda-todo-ignore-with-date nil)
  (org-agenda-todo-ignore-deadlines nil)
  (org-agenda-todo-ignore-scheduled nil)
  (org-agenda-todo-ignore-timestamp nil)
  (org-agenda-skip-additional-timestamps-same-entry t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-insert-diary-extract-time t)
  (org-agenda-include-diary nil)
  (org-agenda-diary-file "~/git/org/diary.org")
  (org-agenda-text-search-extra-files (quote (agenda-archives)))
  (org-agenda-repeating-timestamp-show-all t)
  (org-agenda-show-all-dates t)
  (org-agenda-sorting-strategy
    (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
            (todo category-up effort-up)
            (tags category-up effort-up)
            (search category-up))))
  (org-agenda-tags-column -102)
  (org-agenda-cmp-user-defined 'bh/agenda-sort)
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-compact-blocks t)
  (org-agenda-sticky t)
  (org-agenda-log-mode-items (quote (closed state)))
  (org-agenda-restriction-lock-highlight-subtree nil)
  (org-agenda-clockreport-parameter-plist
    '(:maxlevel 5 :block t :tstart t :tend t :emphasize t :link t :narrow 80 :indent t :formula nil :timestamp t :level
5 :tcolumns nil :formatter nil))
  (org-agenda-current-time-string "← now")
  (org-agenda-time-grid ;; Format is changed from 9.1
    '((daily today require-timed)
      (0900 01000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
       "-"
       "────────────────"))
  (org-agenda-clock-consistency-checks
    (quote (:max-duration "4:00"
            :min-duration 0
            :max-gap 0
            :gap-ok-around ("4:00"))))
  (org-agenda-custom-commands
    (quote (("N" "Notes" tags "NOTE"
              ((org-agenda-overriding-header "Notes")
               (org-tags-match-list-sublevels t)))
            ("h" "Habits" tags-todo "STYLE=\"habit\""
              ((org-agenda-overriding-header "Habits")
               (org-agenda-sorting-strategy
                '(todo-state-down effort-up category-keep))))
            (" " "Agenda"
              ((agenda "" nil)
               (tags "REFILE"
                 ((org-agenda-overriding-header "Tasks to Refile")
                  (org-tags-match-list-sublevels nil)))
               (tags-todo "-CANCELLED/!"
                 ((org-agenda-overriding-header "Stuck Projects")
                  (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                  (org-agenda-sorting-strategy
                    '(category-keep))))
               (tags-todo "-HOLD-CANCELLED/!"
                 ((org-agenda-overriding-header "Projects")
                  (org-agenda-skip-function 'bh/skip-non-projects)
                  (org-tags-match-list-sublevels 'indented)
                  (org-agenda-sorting-strategy
                    '(category-keep))))
               (tags-todo "-CANCELLED/!NEXT"
                 ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                    ""
                                                    " (including WAITING and SCHEDULED tasks)")))
                  (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                  (org-tags-match-list-sublevels t)
                  (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-sorting-strategy
                    '(todo-state-down effort-up category-keep))))
               (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                 ((org-agenda-overriding-header (concat "Project Subtasks"
                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                   ""
                                                   " (including WAITING and SCHEDULED tasks)")))
                  (org-agenda-skip-function 'bh/skip-non-project-tasks)
                  (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-sorting-strategy
                    '(category-keep))))
               (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                 ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                   ""
                                                   " (including WAITING and SCHEDULED tasks)")))
                  (org-agenda-skip-function 'bh/skip-project-tasks)
                  (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-sorting-strategy
                    '(category-keep))))
               (tags-todo "-CANCELLED+WAITING|HOLD/!"
                 ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                   ""
                                                   " (including WAITING and SCHEDULED tasks)")))
                  (org-agenda-skip-function 'bh/skip-non-tasks)
                  (org-tags-match-list-sublevels nil)
                  (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
               (tags "-REFILE/"
                 ((org-agenda-overriding-header "Tasks to Archive")
                  (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                  (org-tags-match-list-sublevels nil))))
               nil))))
  ;; visuals
  (org-src-fontify-natively t)

  :init
  (org-crypt-use-before-save-magic)
  (org-clock-persistence-insinuate)
  (add-hook 'org-clock-out-hook 'pfault/remove-empty-drawer-on-clock-out 'append)
  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
            'append)
  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
            'append)
  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
            'append)
  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
            'append)
  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
            'append)
  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
            'append)
  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
            'append)
  (add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)
  (add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
  (add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)
  (add-hook 'org-clock-in-hook '(lambda ()
                  (setq org-mode-line-string (pfault/task-clocked-time))
                  (run-at-time 0 60 '(lambda ()
                                       (setq org-mode-line-string (pfault/task-clocked-time))
                                       (force-mode-line-update)))
                  (force-mode-line-update))
                'append)
  (add-hook 'org-mode-hook 'turn-on-flyspell 'append)
  (add-hook 'org-mode-hook
            '(lambda ()
               ;; Undefine C-c [ and C-c ] since this breaks my
               ;; org-agenda files when directories are include It
               ;; expands the files in the directories individually
               (org-defkey org-mode-map "\C-c[" 'undefined)
               (org-defkey org-mode-map "\C-c]" 'undefined)
               (org-defkey org-mode-map "\C-c;" 'undefined)
               (org-defkey org-mode-map "\C-c\C-x\C-q" 'undefined))
            'append)

  (add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c M-o") 'bh/mail-subtree))
          'append)

  (defun nexus-org-mode-setup ()
    (setq fill-column 80
          whitespace-action '(auto-cleanup))

    (setcar (nthcdr 4 org-emphasis-regexp-components) 20)
    (org-set-emph-re 'org-emphasis-regexp-components
                     org-emphasis-regexp-components)

    (nexus-display-fill-column)
    (nexus-display-indentation)
    (nexus-display-line-numbers)
    (flyspell-mode)
    (smartparens-mode +1)
    (visual-line-mode +1)
    (whitespace-mode +1))

  :preface
  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
      (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
    )
  (defun pfault/get-today-diary ()
    (concat private-directory
      (format-time-string "diary/%Y/%m/%Y-%m-%d.org" (current-time))))

  (defun pfault/get-yesterday-diary ()
    (concat private-directory
      (format-time-string "diary/%Y/%m/%Y-%m-%d.org" (time-add (current-time) (* -24 3600)))))

  (defun pfault/get-diary-from-cal ()
    (concat private-directory
      (format-time-string "diary/%Y/%m/%Y-%m-%d.org"
        (apply 'encode-time (parse-time-string (concat (org-read-date) " 00:00"))))))

  (defun pfault/open-org-file (fname)
    (switch-to-buffer (find-file-noselect fname)))

  (defun pfault/org-get-time ()
    (format-time-string "<%H:%M>" (current-time)))

  (defun pfault/code-metadata ()
    (concat ":" (projectile-project-name) ":"))

  (defun pfault/org-clock-out-and-save-when-exit ()
    "Save buffers and stop clocking when kill emacs."
    (ignore-errors (org-clock-out) t)
    (save-some-buffers t))

  (defun pfault/task-clocked-time ()
    "Return a string with the clocked time and effort, if any"
    (interactive)
    (let* ((clocked-time (org-clock-get-clocked-time))
           (h (truncate clocked-time 60))
           (m (mod clocked-time 60))
           (work-done-str (format "%d:%02d" h m)))
      (if org-clock-effort
          (let* ((effort-in-minutes
                  (org-duration-to-minutes org-clock-effort))
                 (effort-h (truncate effort-in-minutes 60))
                 (effort-m (truncate (mod effort-in-minutes 60)))
                 (effort-str (format "%d:%02d" effort-h effort-m)))
            (format "%s/%s" work-done-str effort-str))
        (format "%s" work-done-str))))

  (defun pfault/remove-empty-drawer-on-clock-out ()
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at (point))))

  (defun bh/hide-other ()
    (interactive)
    (save-excursion
      (org-back-to-heading 'invisible-ok)
      (hide-other)
      (org-cycle)
      (org-cycle)
      (org-cycle)))

  (defun bh/set-truncate-lines ()
    "Toggle value of truncate-lines and refresh window display."
    (interactive)
    (setq truncate-lines (not truncate-lines))
    ;; now refresh window display (an idiom from simple.el):
    (save-excursion
      (set-window-start (selected-window)
                        (window-start (selected-window)))))

  (defun bh/make-org-scratch ()
    (interactive)
    (find-file "/tmp/publish/scratch.org")
    (gnus-make-directory "/tmp/publish"))

  (defun bh/switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))

  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (defun bh/clock-in-to-next (kw)
    "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
    (when (not (and (boundp 'org-capture-mode) org-capture-mode))
      (cond
       ((and (member (org-get-todo-state) (list "TODO"))
             (bh/is-task-p))
        "NEXT")
       ((and (member (org-get-todo-state) (list "NEXT"))
             (bh/is-project-p))
        "TODO"))))

  (defun bh/find-project-task ()
    "Move point to the parent (project) task if any"
    (save-restriction
      (widen)
      (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (goto-char parent-task)
        parent-task)))

  (defun bh/punch-in (arg)
    "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
    (interactive "p")
    (setq bh/keep-clock-running t)
    (if (equal major-mode 'org-agenda-mode)
        ;;
        ;; We're in the agenda
        ;;
        (let* ((marker (org-get-at-bol 'org-hd-marker))
               (tags (org-with-point-at marker (org-get-tags-at))))
          (if (and (eq arg 4) tags)
              (org-agenda-clock-in '(16))
            (bh/clock-in-organization-task-as-default)))
      ;;
      ;; We are not in the agenda
      ;;
      (save-restriction
        (widen)
                                        ; Find the tags on the current task
        (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
            (org-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))))

  (defun bh/punch-out ()
    (interactive)
    (setq bh/keep-clock-running nil)
    (when (org-clock-is-active)
      (org-clock-out))
    (org-agenda-remove-restriction-lock))

  (defun bh/clock-in-default-task ()
    (save-excursion
      (org-with-point-at org-clock-default-task
        (org-clock-in))))

  (defun bh/clock-in-parent-task ()
    "Move point to the parent (project) task if any and clock in"
    (let ((parent-task))
      (save-excursion
        (save-restriction
          (widen)
          (while (and (not parent-task) (org-up-heading-safe))
            (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
              (setq parent-task (point))))
          (if parent-task
              (org-with-point-at parent-task
                (org-clock-in))
            (when bh/keep-clock-running
              (bh/clock-in-default-task)))))))

  (defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

  (defun bh/clock-in-organization-task-as-default ()
    (interactive)
    (org-with-point-at (org-id-find bh/organization-task-id 'marker)
      (org-clock-in '(16))))

  (defun bh/clock-out-maybe ()
    (when (and bh/keep-clock-running
               (not org-clock-clocking-in)
               (marker-buffer org-clock-default-task)
               (not org-clock-resolving-clocks-due-to-idleness))
      (bh/clock-in-parent-task)))

  (defun bh/clock-in-task-by-id (id)
    "Clock in a task by id"
    (org-with-point-at (org-id-find id 'marker)
      (org-clock-in nil)))

  (defun bh/clock-in-last-task (arg)
    "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
    (interactive "p")
    (let ((clock-in-to-task
           (cond
            ((eq arg 4) org-clock-default-task)
            ((and (org-clock-is-active)
                  (equal org-clock-default-task (cadr org-clock-history)))
             (caddr org-clock-history))
            ((org-clock-is-active) (cadr org-clock-history))
            ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
            (t (car org-clock-history)))))
      (widen)
      (org-with-point-at clock-in-to-task
        (org-clock-in nil))))

  (defun bh/is-project-p ()
    "Any task with a todo keyword subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))

  (defun bh/is-project-subtree-p ()
    "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
    (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                (point))))
      (save-excursion
        (bh/find-project-task)
        (if (equal (point) task)
            nil
          t))))

  (defun bh/is-task-p ()
    "Any task with a todo keyword and no subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task (not has-subtask)))))

  (defun bh/is-subproject-p ()
    "Any task which is a subtask of another project"
    (let ((is-subproject)
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (while (and (not is-subproject) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq is-subproject t))))
      (and is-a-task is-subproject)))

  (defun bh/list-sublevels-for-projects-indented ()
    "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
    (if (marker-buffer org-agenda-restrict-begin)
        (setq org-tags-match-list-sublevels 'indented)
      (setq org-tags-match-list-sublevels nil))
    nil)

  (defun bh/list-sublevels-for-projects ()
    "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
    (if (marker-buffer org-agenda-restrict-begin)
        (setq org-tags-match-list-sublevels t)
      (setq org-tags-match-list-sublevels nil))
    nil)

  (defvar bh/hide-scheduled-and-waiting-next-tasks t)

  (defun bh/toggle-next-task-display ()
    (interactive)
    (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
    (when  (equal major-mode 'org-agenda-mode)
      (org-agenda-redo))
    (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

  (defun bh/skip-stuck-projects ()
    "Skip trees that are not stuck projects"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (bh/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next ))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  nil
                next-headline)) ; a stuck project, has subtasks but no next task
          nil))))

  (defun bh/skip-non-stuck-projects ()
    "Skip trees that are not stuck projects"
    ;; (bh/list-sublevels-for-projects-indented)
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (bh/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next ))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  next-headline
                nil)) ; a stuck project, has subtasks but no next task
          next-headline))))

  (defun bh/skip-non-projects ()
    "Skip trees that are not projects"
    ;; (bh/list-sublevels-for-projects-indented)
    (if (save-excursion (bh/skip-non-stuck-projects))
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((bh/is-project-p)
              nil)
             ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
              nil)
             (t
              subtree-end))))
      (save-excursion (org-end-of-subtree t))))

  (defun bh/skip-non-tasks ()
    "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((bh/is-task-p)
          nil)
         (t
          next-headline)))))

  (defun bh/skip-project-trees-and-habits ()
    "Skip trees that are projects"
    (save-restriction
      (widen)
      (let ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((bh/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-projects-and-habits-and-single-tasks ()
    "Skip trees that are projects, tasks that are habits, single non-project tasks"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((org-is-habit-p)
          next-headline)
         ((and bh/hide-scheduled-and-waiting-next-tasks
               (member "WAITING" (org-get-tags-at)))
          next-headline)
         ((bh/is-project-p)
          next-headline)
         ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
          next-headline)
         (t
          nil)))))

  (defun bh/skip-project-tasks-maybe ()
    "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (next-headline (save-excursion (or (outline-next-heading) (point-max))))
             (limit-to-project (marker-buffer org-agenda-restrict-begin)))
        (cond
         ((bh/is-project-p)
          next-headline)
         ((org-is-habit-p)
          subtree-end)
         ((and (not limit-to-project)
               (bh/is-project-subtree-p))
          subtree-end)
         ((and limit-to-project
               (bh/is-project-subtree-p)
               (member (org-get-todo-state) (list "NEXT")))
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-project-tasks ()
    "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((bh/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         ((bh/is-project-subtree-p)
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-non-project-tasks ()
    "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((bh/is-project-p)
          next-headline)
         ((org-is-habit-p)
          subtree-end)
         ((and (bh/is-project-subtree-p)
               (member (org-get-todo-state) (list "NEXT")))
          subtree-end)
         ((not (bh/is-project-subtree-p))
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-projects-and-habits ()
    "Skip trees that are projects and tasks that are habits"
    (save-restriction
      (widen)
      (let ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((bh/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-non-subprojects ()
    "Skip trees that are not projects"
    (let ((next-headline (save-excursion (outline-next-heading))))
      (if (bh/is-subproject-p)
          nil
        next-headline)))

  (defun bh/skip-non-archivable-tasks ()
    "Skip trees that are not available for archiving"
    (save-restriction
      (widen)
      ;; Consider only tasks with done todo headings as archivable candidates
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
            (subtree-end (save-excursion (org-end-of-subtree t))))
        (if (member (org-get-todo-state) org-todo-keywords-1)
            (if (member (org-get-todo-state) org-done-keywords)
                (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                       (a-month-ago (* 60 60 24 (+ daynr 1)))
                       (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-
ago))))
                       (this-month (format-time-string "%Y-%m-" (current-time)))
                       (subtree-is-current (save-excursion
                                             (forward-line 1)
                                             (and (< (point) subtree-end)
                                                  (re-search-forward (concat last-month "\\|" this-month) subtree-end t
)))))
                  (if subtree-is-current
                      subtree-end ; Has a date in this month or last month, skip it
                    nil))  ; available to archive
              (or subtree-end (point-max)))
          next-headline))))

  (defun bh/org-todo (arg)
    (interactive "p")
    (if (equal arg 4)
        (save-restriction
          (bh/narrow-to-org-subtree)
          (org-show-todo-tree nil))
      (bh/narrow-to-org-subtree)
      (org-show-todo-tree nil)))

  (defun bh/widen ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-agenda-remove-restriction-lock)
          (when org-agenda-sticky
            (org-agenda-redo)))
      (widen)))

  (defun bh/restrict-to-file-or-follow (arg)
    "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
    (interactive "p")
    (if (equal arg 4)
        (org-agenda-follow-mode)
      (widen)
      (bh/set-agenda-restriction-lock 4)
      (org-agenda-redo)
      (beginning-of-buffer)))

  (defun bh/narrow-to-org-subtree ()
    (widen)
    (org-narrow-to-subtree)
    (save-restriction
      (org-agenda-set-restriction-lock)))

  (defun bh/narrow-to-subtree ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-with-point-at (org-get-at-bol 'org-hd-marker)
            (bh/narrow-to-org-subtree))
          (when org-agenda-sticky
            (org-agenda-redo)))
      (bh/narrow-to-org-subtree)))

  (defun bh/narrow-up-one-org-level ()
    (widen)
    (save-excursion
      (outline-up-heading 1 'invisible-ok)
      (bh/narrow-to-org-subtree)))

  (defun bh/get-pom-from-agenda-restriction-or-point ()
    (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
        (org-get-at-bol 'org-hd-marker)
        (and (equal major-mode 'org-mode) (point))
        org-clock-marker))

  (defun bh/narrow-up-one-level ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
            (bh/narrow-up-one-org-level))
          (org-agenda-redo))
      (bh/narrow-up-one-org-level)))

  (defun bh/narrow-to-org-project ()
    (widen)
    (save-excursion
      (bh/find-project-task)
      (bh/narrow-to-org-subtree)))

  (defun bh/narrow-to-project ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
            (bh/narrow-to-org-project)
            (save-excursion
              (bh/find-project-task)
              (org-agenda-set-restriction-lock)))
          (org-agenda-redo)
          (beginning-of-buffer))
      (bh/narrow-to-org-project)
      (save-restriction
        (org-agenda-set-restriction-lock))))

  (defvar bh/project-list nil)

  (defun bh/view-next-project ()
    (interactive)
    (let (num-project-left current-project)
      (unless (marker-position org-agenda-restrict-begin)
        (goto-char (point-min))
                                        ; Clear all of the existing markers on the list
        (while bh/project-list
          (set-marker (pop bh/project-list) nil))
        (re-search-forward "Tasks to Refile")
        (forward-visible-line 1))

                                        ; Build a new project marker list
      (unless bh/project-list
        (while (< (point) (point-max))
          (while (and (< (point) (point-max))
                      (or (not (org-get-at-bol 'org-hd-marker))
                          (org-with-point-at (org-get-at-bol 'org-hd-marker)
                            (or (not (bh/is-project-p))
                                (bh/is-project-subtree-p)))))
            (forward-visible-line 1))
          (when (< (point) (point-max))
            (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
          (forward-visible-line 1)))

                                        ; Pop off the first marker on the list and display
      (setq current-project (pop bh/project-list))
      (when current-project
        (org-with-point-at current-project
          (setq bh/hide-scheduled-and-waiting-next-tasks nil)
          (bh/narrow-to-project))
                                        ; Remove the marker
        (setq current-project nil)
        (org-agenda-redo)
        (beginning-of-buffer)
        (setq num-projects-left (length bh/project-list))
        (if (> num-projects-left 0)
            (message "%s projects left to view" num-projects-left)
          (beginning-of-buffer)
          (setq bh/hide-scheduled-and-waiting-next-tasks t)
          (error "All projects viewed")))))

  (defun bh/set-agenda-restriction-lock (arg)
    "Set restriction lock to current task subtree or file if prefix is specified"
    (interactive "p")
    (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
           (tags (org-with-point-at pom (org-get-tags-at))))
      (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
        (save-restriction
          (cond
           ((and (equal major-mode 'org-agenda-mode) pom)
            (org-with-point-at pom
              (org-agenda-set-restriction-lock restriction-type))
            (org-agenda-redo))
           ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
            (org-agenda-set-restriction-lock 'file))
           (pom
            (org-with-point-at pom
              (org-agenda-set-restriction-lock restriction-type))))))))

  (defun bh/agenda-sort (a b)
    "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
    (let (result num-a num-b)
      (cond
                                        ; time specific items are already sorted first by org-agenda-sorting-strategy

                                        ; non-deadline and non-scheduled items next
       ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

                                        ; deadlines for today next
       ((bh/agenda-sort-test 'bh/is-due-deadline a b))

                                        ; late deadlines next
       ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

                                        ; scheduled items for today next
       ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

                                        ; late scheduled items next
       ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

                                        ; pending deadlines last
       ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

                                        ; finally default to unsorted
       (t (setq result nil)))
      result))

  (defmacro bh/agenda-sort-test (fn a b)
    "Test for agenda sort"
    `(cond
                                        ; if both match leave them unsorted
      ((and (apply ,fn (list ,a))
            (apply ,fn (list ,b)))
       (setq result nil))
                                        ; if a matches put a first
      ((apply ,fn (list ,a))
       (setq result -1))
                                        ; otherwise if b matches put b first
      ((apply ,fn (list ,b))
       (setq result 1))
                                        ; if none match leave them unsorted
      (t nil)))

  (defmacro bh/agenda-sort-test-num (fn compfn a b)
    `(cond
      ((apply ,fn (list ,a))
       (setq num-a (string-to-number (match-string 1 ,a)))
       (if (apply ,fn (list ,b))
           (progn
             (setq num-b (string-to-number (match-string 1 ,b)))
             (setq result (if (apply ,compfn (list num-a num-b))
                              -1
                            1)))
         (setq result -1)))
      ((apply ,fn (list ,b))
       (setq result 1))
      (t nil)))

  (defun bh/is-not-scheduled-or-deadline (date-str)
    (and (not (bh/is-deadline date-str))
         (not (bh/is-scheduled date-str))))

  (defun bh/is-due-deadline (date-str)
    (string-match "Deadline:" date-str))

  (defun bh/is-late-deadline (date-str)
    (string-match "\\([0-9]*\\) d\. ago:" date-str))

  (defun bh/is-pending-deadline (date-str)
    (string-match "In \\([^-]*\\)d\.:" date-str))

  (defun bh/is-deadline (date-str)
    (or (bh/is-due-deadline date-str)
        (bh/is-late-deadline date-str)
        (bh/is-pending-deadline date-str)))

  (defun bh/is-scheduled (date-str)
    (or (bh/is-scheduled-today date-str)
        (bh/is-scheduled-late date-str)))

  (defun bh/is-scheduled-today (date-str)
    (string-match "Scheduled:" date-str))

  (defun bh/is-scheduled-late (date-str)
    (string-match "Sched\.\\(.*\\)x:" date-str))

  (defun bh/show-org-agenda ()
    (interactive)
    (if org-agenda-sticky
        (switch-to-buffer "*Org Agenda( )*")
      (switch-to-buffer "*Org Agenda*"))
    (delete-other-windows))

  (defvar bh/insert-inactive-timestamp t)

  (defun bh/toggle-insert-inactive-timestamp ()
    (interactive)
    (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
    (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

  (defun bh/insert-inactive-timestamp ()
    (interactive)
    (org-insert-time-stamp nil t t nil nil nil))

  (defun bh/insert-heading-inactive-timestamp ()
    (save-excursion
      (when bh/insert-inactive-timestamp
        (org-return)
        (org-cycle)
        (bh/insert-inactive-timestamp))))

  (defun bh/mark-next-parent-tasks-todo ()
    "Visit each parent task and change NEXT states to TODO"
    (let ((mystate (or (and (fboundp 'org-state)
                            state)
                       (nth 2 (org-heading-components)))))
      (when mystate
        (save-excursion
          (while (org-up-heading-safe)
            (when (member (nth 2 (org-heading-components)) (list "NEXT"))
              (org-todo "TODO")))))))

  (defun bh/mail-subtree ()
    (interactive)
    (org-mark-subtree)
    (org-mime-subtree))

  (defhydra hydra-org-clock (:color blue :hint nil)
    "
Clock   In/out^     ^Edit^   ^Summary     (_?_)
-----------------------------------------
        _i_n         _e_dit   _g_oto entry
        _c_ontinue   _q_uit   _d_isplay
        _o_ut        ^ ^      _r_eport
      "
    ("i" org-clock-in)
    ("o" org-clock-out)
    ("c" org-clock-in-last)
    ("e" org-clock-modify-effort-estimate)
    ("q" org-clock-cancel)
    ("g" org-clock-goto)
    ("d" org-clock-display)
    ("r" org-clock-report)
    ("?" (org-info "Clocking commands")))

  (defhydra hydra-org-agenda-clock (:color blue :hint nil)
    "
Clock   In/out^
-----------------------------------------
        _i_n
        _g_oto entry
        _o_ut
        _q_uit
      "
    ("i" org-agenda-clock-in)
    ("o" org-agenda-clock-out)
    ("q" org-agenda-clock-cancel)
    ("g" org-agenda-clock-goto))

  :config
  (require 'org-mouse)
  (use-package org-cliplink
    :bind ("C-x c" . org-cliplink))
  ;; load org-protocol for capturing websites
  (require 'org-protocol)
  ;; load babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (ditaa . t)
     (C . t)
     (emacs-lisp . t)
     (python . t)
     (shell . t)
     (js . t)
     (ledger . t)
     (org . t)
     (plantuml . t)
     (latex . t)
     (R . t)))
  ;; Pretty bullets
  (use-package org-bullets
    :custom (org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" ""))
    :hook (org-mode . org-bullets-mode))
  ;; hugo
  (use-package ox-hugo
    :after ox
    :custom
    (org-blackfriday--org-element-string '((src-block . "Code")
                                           (table . "Table")
                                           (figure . "Figure"))))
  ;; Download Drag&Drop images
  (use-package org-download)
  (setq org-id-locations-file
        (expand-file-name ".org-id-locations" org-directory))
  (with-eval-after-load 'hydra
    (eval-and-compile
      (defun hot-expand (str &optional mod)
        "Expand org template."
        (let (text)
          (when (region-active-p)
            (setq text (buffer-substring (region-beginning) (region-end)))
            (delete-region (region-beginning) (region-end)))
          (insert str)
          (org-try-structure-completion)
          (when mod (insert mod) (forward-line))
          (when text (insert text)))))

    (defhydra hydra-org-template (:color blue :hint nil)
      (format "   %s^^     %s^^^^       %s^^^^^        %s
%s
  %s _e_lisp      %s _h_ugo        %s plant_u_ml      %s _s_ource
  %s _r_uby       %s _c_aption     %s La_t_ex         %s _n_ote
  %s _f_ish       %s _l_ink        %s i_P_ython       %s _i_nfo
  %s _b_ash       %s %s^^^^^       %s %s^^^^          %s qu_o_te
  %s _g_o
  %s _y_aml
%s
                     %s quit%s insert
"
              (concat (propertize "((" 'face `(:foreground "#6272a4"))
                      (propertize "CODE" 'face `(:foreground "#ff79c6" :weight bold))
                      (propertize "))" 'face `(:foreground "#6272a4")))
              (concat (propertize "((" 'face `(:foreground "#6272a4"))
                      (propertize "META" 'face `(:foreground "#ff79c6" :weight bold))
                      (propertize "))" 'face `(:foreground "#6272a4")))
              (concat (propertize "((" 'face `(:foreground "#6272a4"))
                      (propertize "DRAW" 'face `(:foreground "#ff79c6" :weight bold))
                      (propertize "))" 'face `(:foreground "#6272a4")))
              (concat (propertize "((" 'face `(:foreground "#6272a4"))
                      (propertize "BLOCK" 'face `(:foreground "#ff79c6" :weight bold))
                      (propertize "))" 'face `(:foreground "#6272a4")))

              (propertize " ──────────────────────────────────────────────────────────── " 'face `(:foreground "#6272a4
"))
              ;; L1
              (all-the-icons-fileicon "emacs" :v-adjust .00001 :height .68 :face '(:foreground "#6272a4"))
              (all-the-icons-material "web" :v-adjust -.1 :height .7 :face '(:foreground "#6272a4"))
              (all-the-icons-material "format_shapes" :v-adjust -.15 :height .7 :face '(:foreground "#6272a4"))
              (all-the-icons-octicon "code" :v-adjust -.05 :height .75  :face '(:foreground "#6272a4"))
              ;; L2
              (all-the-icons-alltheicon "ruby-alt" :v-adjust .0505 :height .7 :face '(:foreground "#6272a4"))
              (all-the-icons-faicon "flag" :v-adjust -.05 :height .69  :face '(:foreground "#6272a4"))
              (all-the-icons-faicon "text-height" :v-adjust -.05 :height .69 :face '(:foreground "#6272a4"))
              (all-the-icons-octicon "light-bulb" :v-adjust -.1 :height .78 :face '(:foreground "#6272a4"))
              ;; L3
              (all-the-icons-alltheicon "script" :v-adjust .05 :height .7 :face '(:foreground "#6272a4"))
              (all-the-icons-faicon "link" :v-adjust -.05 :height .69 :face '(:foreground "#6272a4"))
              (all-the-icons-fileicon "test-python" :v-adjust -.1 :height .75 :face '(:foreground "#6272a4"))
              (all-the-icons-faicon "info-circle" :v-adjust -.1 :height .72 :face '(:foreground "#6272a4"))
              ;; L4
              (all-the-icons-alltheicon "script" :v-adjust .05 :height .7 :face '(:foreground "#6272a4"))
              (all-the-icons-fileicon "test-python" :v-adjust -.1 :height .7 :face '(:foreground "#282a36")) ;; dummy
              (propertize "link" 'face `(:foreground "#282a36"))
              (all-the-icons-fileicon "test-python" :v-adjust -.1 :height .7 :face '(:foreground "#282a36")) ;; dummy
              (propertize "latex" 'face `(:foreground "#282a36"))
              (all-the-icons-faicon "quote-right" :v-adjust -.05 :height .65 :face '(:foreground "#6272a4"))
              ;; L5
              (all-the-icons-fileicon "go" :v-adjust -.1 :height .75 :face '(:foreground "#6272a4"))
              ;; L6
              (all-the-icons-octicon "settings" :v-adjust -.1 :height .75 :face '(:foreground "#6272a4"))
              ;; Draw
              (propertize " ┌──────────────────────────────────────────────────────────┘ " 'face `(:foreground "#6272a4
"))
              (propertize "[_q_]:" 'face `(:foreground "#6272a4"))
              (propertize ", [_<_]:" 'face `(:foreground "#6272a4"))
              )
      ("s" (hot-expand "<s"))
      ("o" (hot-expand "<q"))
      ("c" (hot-expand "<c"))
      ("t" (hot-expand "<L"))
      ("c" (insert "#+CAPTION: "))
      ("l" (insert "#+NAME: "))
      ("n" (insert "#+BEGIN_NOTE\n\n#+END_NOTE"))
      ("i" (insert "#+BEGIN_INFO\n\n#+END_INFO"))
      ("h" (insert ":PROPERTIES:\n:EXPORT_FILE_NAME:\n:EXPORT_HUGO_SECTION: pages\n:EXPORT_HUGO_TAGS:\n:EXPORT_HUGO_CAT
EGORIES:\n:END:"))
      ("e" (hot-expand "<s" "emacs-lisp"))
      ("f" (hot-expand "<s" "fish"))
      ("b" (hot-expand "<s" "bash"))
      ("y" (hot-expand "<s" "yaml"))
      ("P" (hot-expand "<s" "ipython :session :exports both :async :cache yes :results raw drawer\n$0"))
      ("g" (hot-expand "<s" "go"))
      ("r" (hot-expand "<s" "ruby"))
      ("S" (hot-expand "<s" "sh"))
      ("u" (hot-expand "<s" "plantuml :file overview.svg :cache yes :cmdline -config \"$HOME/Private/style.uml\" :async
"))
      ("<" self-insert-command)
      ("q" nil))
  (bind-key "<"
            (lambda () (interactive)
              (if (or (region-active-p) (looking-back "^\s*" 1))
                  (hydra-org-template/body)
                  (self-insert-command 1)))
              org-mode-map)
    )
  )

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(provide 'nexus-org-mode)
;;; nexus-org-mode.el ends here
