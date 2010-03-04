;;; utilities.el --- my utility functions

;; Copyright (C) 2010  Cameron Desautels
;; Author: Cameron Desautels <camdez@gmail.com>, others as noted

;; Escape all double-quotes (") in the region
(fset 'escape-region-double-quotes
   "\C-xnn\C-[<\C-[%\"\C-m\\\"\C-m!\C-[>\C-xnw")

;; Should make this uncomment if the line is already commented. Or at
;; least do so when an argument is given.
(defun comment-line ()
  "Comments out the line containing point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (end-of-line)
      (comment-region beg (point)))))

(defun count-words-buffer ()
  "Count the number of words in the current buffer and print the
result in the minibuffer."
  (interactive)
  (count-words-region (point-min) (point-max)))

(defun count-words-region (region-start region-end)
  "Count the number of words in the region and print the result in the
minibuffer"
  (interactive "r")
  (save-excursion
    (save-restriction
      (widen)
      (let ((count 0))
        (goto-char region-start)
        (while (< (point) region-end)
          (forward-word 1)
          (setq count (1+ count)))
        (message "Contains %d words." count)))))

(defun count-words-buffer-using-wc nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun duplicate-line ()
  "Duplicate the current line below the original, leaving point
  and mark in place."
  (interactive)
  (save-excursion
    (let ((beg (progn
                 (beginning-of-line)
                 (point)))
          (end (progn
                 (end-of-line)
                 (point))))
      (copy-region-as-kill beg end)
      (newline)
      (yank))))

(defun outline-increase-region-depth (region-start region-end &optional depth)
  "Increase the depth of header lines in region by DEPTH.
  If no DEPTH is given, defaults to 1."
  (interactive "r\np")
  (save-excursion
    (let ((indent-string (make-string depth ?*)))
      (goto-char region-start)
      (beginning-of-line)
      (while (< (point) region-end)
        (if (looking-at "\*")
            (insert indent-string))
        (forward-line)))))

(defun outline-newline-and-indent (&optional p)
  "Inserts a newline and a new header at the current outline level.
With a prefix, makes a new header at the parent level."
  (interactive "P")
  (let ((current-pos (point)))
    ;; if we were able to go up the outline
    (unless (condition-case nil
                (outline-up-heading 1) ; this func. always returns nil
              (error t))            ; so we make it return t on error
        ; then: we are not at the top level
      (let ((up-level-prefix
             (buffer-substring-no-properties
              (progn
                (beginning-of-line)
                (point))
              (progn
                (forward-word 1)  ; leave point just 
                (backward-word 1) ;  before 1st word
                (point)))))
        (goto-char current-pos)
        (newline)
        (insert up-level-prefix))
      ;; else: we were at the top level
      (progn
        (goto-char current-pos)
        (newline)
        (insert ? ))))
  (beginning-of-line)
  (unless p
    (insert ?*))
  (end-of-line))

(add-hook 'outline-mode-hook
          '(lambda ()
             (define-key outline-mode-map "\C-j"
               'outline-newline-and-indent)))

(defun insert-c-for-loop (variable max)
  "Inserts a C-style for loop in which the given VARIABLE is \
incremented from 0 to MAX."
  (interactive "sVariable name for for loop:\nnValue to increment \
to:")
  (beginning-of-line)
  (c-indent-line)
  (insert (format "for (int %s = 0; %s < %d; %s++)"
                  variable variable max variable))
  (newline-and-indent)
  (insert "{\n\n}")
  (c-indent-line)
  (forward-line -1)                   ; go back a line
  (c-indent-line))

(defun reread-config-file ()
  "Reread .emacs file"
  (interactive)
  (load-file user-init-file))

(defun kill-buffer-other-window ()
  "Kill the buffer currently displayed in the other window."
  (interactive)
  (other-window 1)
  (kill-buffer nil)
  (other-window 1))

(defun delete-window-replacement (&optional p)
  "Kill current window.  If called with PREFIX, kill the buffer too."
  (interactive "P")
  (if p
      (kill-buffer nil))
  (delete-window))

(defun delete-other-windows-replacement (&optional p)
  "Make the selected window fill its frame.  If called with PREFIX,
kill all other visible buffers."
  (interactive "P")
  (if p
      (dolist (window (window-list))
        (unless (equal (window-buffer window) (current-buffer))
          (kill-buffer (window-buffer window)))))
  (delete-other-windows))

(defun buffer-file-basename (buf)
  "Return the filename, with extension removed, of the file associated
with the buffer `buf'.  Returns nil if there is no file associated
with `buf'."
  (let ((filename (buffer-file-name buf)))
    (if filename
        (file-name-nondirectory (file-name-sans-extension filename)))))

(defun buffers-same-file-basename (buf)
  "Return the list of buffers which are associated with files with the
same basename (filename sans extension) as the file the buffer `buf'
is associated with."
  (let ((basename (buffer-file-basename buf)))
    (remove nil
            (mapcar #'(lambda (obuf)
                        (unless
                            (or (eq buf obuf)
                                (not (string= basename (buffer-file-basename obuf))))
                          obuf))
                    (buffer-list)))))

(defun next-buffer-same-file-basename ()
  "Switch to the next buffer (if one exists) associated with a file of
the same basename (filename sans extension) as the ile the current
buffer is associated with."
  (interactive)
  (let ((next-buffer (car (buffers-same-file-basename (current-buffer)))))
    (if next-buffer
        (switch-to-buffer next-buffer)
      (message "No buffers exist with same file basename"))))

(defun narrow-to-php ()
 "Narrow current buffer to the PHP tag currently enclosing point."
 (interactive)
 (save-excursion
   (widen)
   (or (search-forward "?>" (point-max) t)
       (error "No PHP tag to narrow to"))
   (let ((end (point)))
     (or (search-backward "<?" (point-min) t)
         (error "This PHP tag seems to not have a beginning tag"))
     (narrow-to-region (point) end))
   (if (fboundp 'php-mode)
       (php-mode))))

;;; STUFF MOSTLY BORROWED FROM OTHERS

;; Insert the date, the time, and the date & time at point.
(defvar insert-time-format "%T"
  "*Format for \\[insert-time] (c.f. 'format-time-string' for how to format).")

(defvar insert-date-format "%Y-%m-%d"
  "*Format for \\[insert-date] (c.f. 'format-time-string' for how to format).")

(defun insert-time ()
  "Insert the current time according to the variable \"insert-time-format\"."
  (interactive "*")
  (insert (format-time-string insert-time-format
			      (current-time))))

(defun insert-date ()
  "Insert the current date according to the variable \"insert-date-format\"."
  (interactive "*")
  (insert (format-time-string insert-date-format
			      (current-time))))

(defun insert-date-and-time ()
  "Insert the current date according to the variable \"insert-date-format\", then a space, then the current time according to the variable \"insert-time-format\"."
  (interactive "*")
  (progn
    (insert-date)
    (insert " ")
    (insert-time)))

;; From Ulrik Jensen
;; Highlighting long lines
(defun highlight-long-lines-toggle ()
  "Toggles highlighting lines with more than 80 characters"
  (interactive)
  (require 'hi-lock)                    ; needed
  (if (member "^.\\{80\\}.+$" (mapcar
                               (lambda (entry)
                                 (car entry)) hi-lock-interactive-patterns))
      (progn
        (unhighlight-regexp "^.\\{80\\}.+$")
        (message "Highlighting long lines off"))
    (highlight-regexp "^.\\{80\\}.+$" 'zmacs-region)
    (message "Highlighting long lines on")))

(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (dabbrev-expand nil)
    (indent-for-tab-command)))

;; (add-hook 'c-mode-common-hook
;;           (function (lambda ()
;;                       (local-set-key (kbd "<tab>") 'indent-or-complete)
;;                       )))

;;(defun indent-or-complete ()
;;      "Complete if point is at end of line, and indent line."
;;      (interactive)
;;      (if (looking-at "$")
;;          (hippie-expand nil))
;;        (indent-for-tab-command))


(defun pretty-lambdas ()
  (interactive)
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun underline-line ()
  "Underline the current line with `=', or underline the previous line
if at the beginning of a line."
  (interactive)
  (let ((cc (current-column)))
    (when (= cc 0)
      (previous-line 1)
      (end-of-line)
      (setq cc (current-column)))
    (newline)
    (dotimes (i cc)
      (insert "="))))


;;; utilities.el ends here
