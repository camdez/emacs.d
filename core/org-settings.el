;;; org-settings.el --- Org-Mode configuration                -*- lexical-binding: t; -*-
;;; Author: Cameron Desautels <camdez@gmail.com>

;;; Commentary:

;; Org-Mode is really its own beast, with a lot of configuration /
;; extension.

;;; Code:

(add-hook 'org-load-hook
          #'(lambda ()
              (require 'org-depend "~/.emacs.d/lib/org-depend")))

(add-hook 'org-load-hook
          #'(lambda ()
              (require 'org-expiry "~/.emacs.d/lib/org-expiry")
              ;; (org-expiry-insinuate)
              (add-to-list 'org-modules 'org-depend)
              (add-to-list 'org-modules 'org-expiry)
              (add-to-list 'org-modules 'org-habit)))

(add-hook 'org-capture-before-finalize-hook
          #'(lambda ()
              (org-expiry-insert-created)))

(defun camdez/org-agenda-mode-hook ()
  (hl-line-mode 1)
  (setq show-trailing-whitespace nil))

(add-hook 'org-agenda-mode-hook
          'camdez/org-agenda-mode-hook)

(require 'org-install nil t)

;; Explore:
;; - org-directory
;; - org-agenda-breadcrumbs-separator " ❱ "
;; - org-agenda-skip-scheduled-if-deadline-is-shown
;; - org-clock-into-drawer

(setq org-default-notes-file "~/Sync/org/notes.org"
      org-babel-load-languages '((emacs-lisp . t)
                                 (shell . t))

      ;; Might also need to run this
      ;; (org-babel-do-load-languages
      ;;  'org-babel-load-languages
      ;;  '((shell . t)))

      org-blank-before-new-entry '((heading . t) (plain-list-item . auto))
      org-clock-idle-time 10
      org-deadline-warning-days 2
      org-ellipsis "…"
      org-enforce-todo-dependencies t
      org-adapt-indentation t
      org-agenda-include-diary t
      org-agenda-restore-windows-after-quit t
      org-agenda-block-separator ?_
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
      org-agenda-start-with-log-mode t
      org-agenda-span 2
      org-agenda-time-grid '((daily today remove-match require-timed)
                             (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000)
                             "......" "----------------")
      org-agenda-archives-mode nil
      org-expiry-inactive-timestamps t
      org-habit-graph-column 50
      org-habit-completed-glyph ?x
      org-habit-today-glyph ??
      org-id-link-to-org-use-id 'create-if-interactive
      org-image-actual-width nil ; take width from image property, if present
      org-startup-with-inline-images t
      org-log-done 'time
      org-log-reschedule 'time
      ;; Here's how I think about priority:
      ;;
      ;; A: MUST do
      ;; B: SHOULD do
      ;; C: GOOD to do, but nice-to-have
      ;; D: PROBABLY GOOD to do, maybe still considering
      org-priority-lowest 68 ; add 'D' option
      org-todo-keywords '((sequence "TODO(t)" "STARTED(s!)" "WAIT(w@)"
                                    "|"
                                    "DONE(d!)" "CANCELED(c@)"))
      org-agenda-custom-commands '(("a" "NEW Agenda for current day or week"
                                    ((agenda) ; TODO: exclude "WAIT" from here
                                     (todo "WAIT"))
                                    nil
                                    ("~/Sync/org/server/agenda.html"))
                                   ("f" "Agenda and Flagged Tasks"
                                    ((agenda "")
                                     (tags-todo "FLAGGED")
                                     (todo "WAIT")
                                     (tags-todo "EASY")))
                                   ("h" "Habits"
                                    ((tags-todo "STYLE=\"habit\"")))
                                   ("n" "Agenda and all TODO's"
                                    ((agenda)
                                     (alltodo)))
                                   ("o" "Tasks older than a month"
                                    ((tags-todo "CREATED<=\"<-1m>\"")
                                     (tags-todo "-CREATED={.+}")))
                                   ("r" "Review"
                                    ;; TODO: things that are overdue
                                    ;; or past schedule.  Probably
                                    ;; didn't miss them, but they
                                    ;; should still be reviewed, if
                                    ;; only to push their dates
                                    ;; forward.
                                    ;;
                                    ;; I could also surface INBOX
                                    ;; things.  Maybe all inboxes
                                    ;; should have a tag that is
                                    ;; inherited?
                                    ((tags-todo "FLAGGED")
                                     (tags-todo "EASY")
                                     (todo "WAIT")
                                     (tags-todo "CREATED<=\"<-1m>\""
                                                ((org-agenda-overriding-header "Old tasks without DEADLINE or SCHEDULED")
                                                 (org-agenda-tags-todo-honor-ignore-options t)
                                                 (org-agenda-todo-ignore-deadlines 'all)
                                                 (org-agenda-todo-ignore-scheduled 'future)))
                                     (tags-todo "-CREATED={.+}"
                                                ((org-agenda-overriding-header "Tasks without CREATED")))
                                     (tags-todo "INBOX"))))
      org-refile-targets '((nil . (:maxlevel . 2)))
      org-capture-templates '(("h" "Habit" entry (file+headline "~/Sync/org/habits.org" "Active")
                               "* TODO [#C] %?\n  SCHEDULED: <%<%F %a> .+1d>\n  :PROPERTIES:\n  :STYLE:    habit\n  :END:\n\n    %i"
                               :empty-lines 1)
                              ("j" "Journal Entry" plain (file+olp+datetree "~/Sync/org/journal.org" "Dailies")
                               "%?\n%i\n"
                               :empty-lines 1
                               :time-prompt t)
                              ("l" "Log Entry" entry (file+olp+datetree "~/Sync/org/personal.org" "Log")
                               "* %? %T\n"
                               :time-prompt t)
                              ("n" "Note" entry (file+headline "~/Sync/org/notes.org" "Inbox")
                               "* %?%(unless (string= \"\" \"%:description\") \"\\\"%:description\\\"\")\n\n  %:link\n\n  %i"
                               :empty-lines 1)
                              ("p" "Post Topic for Blog" entry (file+headline "~/Sync/org/posts.org" "Inbox")
                               "* %?\n  %i")
                              ("r" "Reading" entry (file+headline "~/Sync/org/reading.org" "In-Progress")
                               "* TODO _%?_ %t--\n")
                              ("t" "Task" entry (file+headline "~/Sync/org/personal.org" "Inbox")
                               "* TODO %?%(unless (string= \"\" \"%:description\") \"\\\"%:description\\\"\")\n\n  %:link\n\n  %i")
                              ("T" "TIL" entry (file+headline "~/Sync/org/til.org" "Inbox")
                               "* %?\n\n%i")
                              ("w" "Work Task" entry (file+headline "~/Sync/org/work.org" "Tasks")
                               "* TODO %?\n  %i"
                               :empty-lines 1)
                              ("W" "Work Log Entry" plain (file+olp+datetree "~/Sync/org/work-log.org")
                               "%?\n%i\n"
                               :empty-lines 1
                               :time-prompt t)))

;; TODO: functional, but probably not the best way to do this
(setq org-html-head-extra
      "<style>
  img { max-width: 600px; margin: auto; }
  body { max-width: 800px; margin: auto; font-family: sans-serif; line-height: 1.5; }
  sup { line-height: 0; } /* don't make lines taller */
  pre.src { background-color: #272822; color: #F8F8F2; line-height: normal; }
  pre.src:before { background-color: #272822; color: #F8F8F2; }
  li { margin-bottom: 0.7em }
  #table-of-contents li { margin-bottom: 0; }
</style>")

;;; org-settings.el ends here
