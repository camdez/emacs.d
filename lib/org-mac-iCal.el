;;; org-mac-iCal.el --- Imports events from iCal.app to the Emacs diary

;; Copyright (C) 2009-2014 Christopher Suckling

;; Author: Christopher Suckling <suckling at gmail dot com>
;; Version: 0.1057.104
;; Keywords: outlines, calendar

;; ********************************************************************************
;; ndw edited the sw_vers regex to make it work with 10.10+
;; ndw extended to support exchange calendars
;; ndw edited omi-checked to handle the possibility of missing Info.plist files
;; ndw added a flag to enable/disable importing Exchange calendars
;; ndw added a list of calendar names as an alternative to Checked
;; ********************************************************************************

;; This file is not part of GNU Emacs.

;; This program is Free Software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file provides the import of events from Mac OS X 10.5 iCal.app
;; into the Emacs diary (it is not compatible with OS X < 10.5). The
;; function org-mac-iCal will import events in all checked iCal.app
;; calendars for the date range org-mac-iCal-range months, centered
;; around the current date.
;;
;; CAVEAT: This function is destructive; it will overwrite the current
;; contents of the Emacs diary.
;;
;; Installation: add (require 'org-mac-iCal) to your .emacs.
;;
;; If you view Emacs diary entries in org-agenda, the following hook
;; will ensure that all-day events are not orphaned below TODO items
;; and that any supplementary fields to events (e.g. Location) are
;; grouped with their parent event
;;
;; (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;        (lambda ()
;;          (goto-char (point-min))
;;          (save-excursion
;;            (while (re-search-forward "^[a-z]" nil t)
;;              (goto-char (match-beginning 0))
;;              (insert "0:00-24:00 ")))
;;          (while (re-search-forward "^ [a-z]" nil t)
;;            (goto-char (match-beginning 0))
;;            (save-excursion
;;              (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;            (insert (match-string 0)))))

;;; Code:

(defcustom org-mac-iCal-range 2
  "The range in months to import iCal.app entries into the Emacs
diary. The import is centered around today's date; thus a value
of 2 imports entries for one month before and one month after
today's date"
  :group 'org-time
  :type 'integer)

(defcustom org-mac-iCal-import-exchange nil
  "Import Exchange calendars? In principle, Exchange calendars are
supported, but there are clearly issues with time zones and the way
Exchange handles repeating/duplicated/declined events.

Sometimes the Exchange .ics file contains several events with the
same UID. Sometimes they have different SEQUENCE numbers. It's
unclear how these are supposed to be handled. At present, they
sometimes occur multiple times in the diary."
  :group 'org-time
  :type 'boolean)

(defcustom org-mac-iCal-calendar-names nil
  "A list of named calendars. If the calendar name list is not empty,
only calendars with titles that match one of the specified names
will be imported. On MacOS 10.14, the Checked property in the
calendar plist file does not seem to be reliable."
  :group 'org-time
  :type '(repeat string))

(defun org-mac-iCal ()
  "Selects checked calendars in iCal.app and imports them into
the the Emacs diary"
  (interactive)

  ;; kill diary buffers then empty diary files to avoid duplicates
  (setq currentBuffer (buffer-name))
  (setq openBuffers (mapcar (function buffer-name) (buffer-list)))
  (omi-kill-diary-buffer openBuffers)
  (with-temp-buffer
    (insert-file-contents diary-file)
    (delete-region (point-min) (point-max))
    (write-region (point-min) (point-max) diary-file))

  ;; determine available calendars
  (setq caldav-folders (directory-files "~/Library/Calendars" 1 ".*caldav$"))

  ;; HACK: Only load this one calendar I want.
  ;; (setq caldav-folders '("/Users/camdez/Library/Calendars/EFD253CA-63B1-4536-9B53-A432C8CFCC06.caldav"))

  (setq caldav-calendars nil)
  (mapc
     (lambda (x)
       (setq caldav-calendars (nconc caldav-calendars (directory-files x 1 ".*calendar$"))))
     caldav-folders)

  (setq exchange-folders (directory-files "~/Library/Calendars" 1 ".*exchange$"))
  (setq exchange-calendars nil)
  (if org-mac-iCal-import-exchange
      (mapc
       (lambda (x)
         (setq exchange-calendars (nconc exchange-calendars (directory-files x 1 ".*calendar$"))))
       exchange-folders))

  (setq local-calendars nil)
  (setq local-calendars (directory-files "~/Library/Calendars" 1 ".*calendar$"))

  ;; HACK: empty this
  ;; (setq local-calendars '())

  ;; ("/Users/camdez/Library/Calendars/2BB91D5B-266E-46D4-A1E9-698F0B18760C.calendar"
  ;;  "/Users/camdez/Library/Calendars/37D765A4-D0FF-4988-8500-F7D722D9BD72.calendar"
  ;;  "/Users/camdez/Library/Calendars/6C06F730-C747-4841-9A3C-35353D7E8351.calendar"
  ;;  "/Users/camdez/Library/Calendars/744D07F8-A831-4482-A7FE-56346BC4214D.calendar"
  ;;  "/Users/camdez/Library/Calendars/823B259B-A191-4ACD-B760-77D1EF58BFDC.calendar"
  ;;  "/Users/camdez/Library/Calendars/9DE101C1-59E5-4D3B-9B95-366DEC28BE13.calendar"
  ;;  "/Users/camdez/Library/Calendars/F8A5DBDA-F76D-46C4-ACC8-E33270673D17.calendar")

  (setq all-calendars (append caldav-calendars exchange-calendars local-calendars))

  ;; parse each calendar's Info.plist to see if calendar is checked in iCal
  (setq all-calendars (delq 'nil (mapcar
                                    (lambda (x)
                                      (omi-checked x))
                                    all-calendars)))

  ;; for each calendar, concatenate individual events into a single ics file
  (with-temp-buffer
    (shell-command "sw_vers" (current-buffer))
    ;; HACK: always run this
    (when t ;;(re-search-backward "\\(10\\.[789]\\)\\|\\(10\\.1[0-9]\\.\\)" nil t)
      (omi-concat-leopard-ics all-calendars)))

  ;; move all caldav and exchange ics files to the same place as local ics files
  (mapc
   (lambda (x)
     (mapc
      (lambda (y)
        (rename-file (concat x "/" y);
                     (concat "~/Library/Calendars/" y)))
      (directory-files x nil ".*ics$")))
   (append caldav-folders exchange-folders))

  ;; check calendar has contents and import
  (setq import-calendars (directory-files "~/Library/Calendars" 1 ".*ics$"))
  (mapc
   (lambda (x)
     (when (/= (nth 7 (file-attributes x 'string)) 0)
       (omi-import-ics x)))
   import-calendars)

  ;; tidy up intermediate files and buffers
  (setq usedCalendarsBuffers (mapcar (function buffer-name) (buffer-list)))
  (omi-kill-ics-buffer usedCalendarsBuffers)
  (setq usedCalendarsFiles (directory-files "~/Library/Calendars" 1 ".*ics$"))
  (omi-delete-ics-file usedCalendarsFiles)

  (org-pop-to-buffer-same-window currentBuffer))

(defun omi-concat-leopard-ics (list)
  "Leopard stores each iCal.app event in a separate ics file.
Whilst useful for Spotlight indexing, this is less helpful for
icalendar-import-file. omi-concat-leopard-ics concatenates these
individual event files into a single ics file"
  (mapc
   (lambda (x)
     (setq omi-leopard-events (directory-files (concat x "/Events") 1 ".*ics$"))
     (with-temp-buffer
       (mapc
        (lambda (y)
          (insert-file-contents (expand-file-name y)))
        omi-leopard-events)
       (write-region (point-min) (point-max) (concat (expand-file-name x) ".ics"))))
   list))

(defun omi-import-ics (string)
  "Imports an ics file into the Emacs diary. First tidies up the
ics file so that it is suitable for import and selects a sensible
date range so that Emacs calendar view doesn't grind to a halt"
  (with-temp-buffer
    (insert-file-contents string)
    (goto-char (point-min))
    (while
        (re-search-forward "^BEGIN:VCALENDAR$" nil t)
      (setq startEntry (match-beginning 0))
      (re-search-forward "^END:VCALENDAR$" nil t)
      (setq endEntry (match-end 0))
      (save-restriction
        (narrow-to-region startEntry endEntry)
        (goto-char (point-min))
        (re-search-forward "\\(^DTSTART;.*:\\)\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)" nil t)
        (if (or (eq (match-string 2) nil) (eq (match-string 3) nil))
            (progn
              (setq yearEntry 1)
              (setq monthEntry 1))
          (setq yearEntry (string-to-number (match-string 2)))
          (setq monthEntry (string-to-number (match-string 3))))
        (setq year (string-to-number (format-time-string "%Y")))
        (setq month (string-to-number (format-time-string "%m")))
        (setq now (list month 1 year))
        (setq entryDate (list monthEntry 1 yearEntry))
        ;; Check to see if this is a repeating event
        (goto-char (point-min))
        (setq isRepeating (re-search-forward "^RRULE:" nil t))
        ;; Delete if outside range and not repeating
        (when (and
               (not isRepeating)
               (> (abs (- (calendar-absolute-from-gregorian now)
                          (calendar-absolute-from-gregorian entryDate)))
                  (* (/ org-mac-iCal-range 2) 30))
          (delete-region startEntry endEntry)))
          (goto-char (point-max))))
    (while
        (re-search-forward "^END:VEVENT$" nil t)
      (delete-blank-lines))
    (goto-line 1)
    (insert "BEGIN:VCALENDAR\n\n")
    (goto-line 2)
    (while
        (re-search-forward "^BEGIN:VCALENDAR$" nil t)
      (replace-match "\n"))
    (goto-line 2)
    (while
        (re-search-forward "^END:VCALENDAR$" nil t)
      (replace-match "\n"))
    (insert "END:VCALENDAR")
    (goto-line 1)
    (delete-blank-lines)
    (while
        (re-search-forward "^END:VEVENT$" nil t)
      (delete-blank-lines))
    (goto-line 1)
    (while
        (re-search-forward "^ORG.*" nil t)
      (replace-match "\n"))
    (goto-line 1)
    (write-region (point-min) (point-max) string))

  (icalendar-import-file string diary-file))

(defun omi-kill-diary-buffer (list)
  (mapc
   (lambda (x)
     (if (string-match "^diary" x)
         (kill-buffer x)))
   list))

(defun omi-kill-ics-buffer (list)
  (mapc
   (lambda (x)
     (if (string-match "ics$" x)
         (kill-buffer x)))
   list))

(defun omi-delete-ics-file (list)
  (mapc
   (lambda (x)
     (delete-file x))
   list))

;;; HACK:
(setq org-mac-iCal-calendar-names '("Cameron Desautels" "Family" "Personal"))

(defvar camdez/org-mac-iCal-calendar-ignore-keys
  '("71CADA49-0F68-438E-A0B1-DF5A1176DAC3" ; Google main calendar
    "3F817BB1-7B0C-4D95-879E-9D088E30E073" ; Google birthdays
    "BFB66651-5895-441B-9C19-DF810DF779AE" ; Google US holidays
    ))

(defun omi-checked (directory)
  "Parse Info.plist in iCal.app calendar folder and determine
whether to include the calendar or not. If
org-mac-iCal-calendar-names is nil, then the calendar is included
if Checked key is 1. If the names list is not nil, the calendar
is included only if it has a matching name."
  (let ((filename (car (directory-files directory 1 "Info.plist"))))
    (if filename
        (let* ((root (xml-parse-file filename))
               (plist (car root))
               (dict (car (xml-get-children plist 'dict)))
               (keys (cdr (xml-node-children dict)))
               (keys (mapcar
                      (lambda (x)
                        (cond ((listp x)
                               x)))
                      keys))
               (keys (delq 'nil keys))
               (checked (car (cddr (lax-plist-get keys '(key nil "Checked")))))
               (title (car (cddr (lax-plist-get keys '(key nil "Title")))))
               ;; HACK:
               (key (car (cddr (lax-plist-get keys '(key nil "Key"))))))
          (message "Key: %s" key)
          (when (and (or
                      (and (eq org-mac-iCal-calendar-names nil) (equal "1" checked))
                      (member title org-mac-iCal-calendar-names))
                     ;; HACK:
                     (not (member key camdez/org-mac-iCal-calendar-ignore-keys)))

            directory))
      nil)))

(provide 'org-mac-iCal)

;;; org-mac-iCal.el ends here
