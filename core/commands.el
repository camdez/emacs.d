;;; commands.el --- my utility commands

;; Copyright (C) 2010  Cameron Desautels
;; Author: Cameron Desautels <camdez@gmail.com>, others as noted

;; Escape all double-quotes (") in the region
(fset 'escape-region-double-quotes
   "\C-xnn\C-[<\C-[%\"\C-m\\\"\C-m!\C-[>\C-xnw")

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(autoload 'copy-from-above-command "misc"
  "Copy characters from previous nonblank line, starting just above point.")

(defalias 'elisp-mode 'emacs-lisp-mode)
(defalias 'cleanup-whitespace 'whitespace-cleanup)

(defun camdez/capitalize-word (&optional arg)
  "Like `capitalize-word', but with universal argument,
capitalize first letter and leave rest of word unchanged.

For those frustrating cases like changing \"fooBarBaz\" ->
\"FooBarBaz\"."
  (interactive "P")
  (if (consp arg)
      (let ((char (capitalize (char-after))))
        (delete-char 1)
        (insert char))
    (capitalize-word (prefix-numeric-value arg))))

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

(defun count-words-buffer (&optional print-message)
  "Count the number of words in the current buffer and print the
result in the minibuffer."
  (interactive "p")
  (count-words-region (point-min) (point-max) print-message))

(defun count-words-region (region-start region-end &optional print-message)
  "Count the number of words in the region and print the result in the
minibuffer."
  (interactive "rp")
  (save-excursion
    (save-restriction
      (widen)
      (let ((count 0))
        (goto-char region-start)
        (while (< (point) region-end)
          (forward-word 1)
          (setq count (1+ count)))
        (when print-message
          (message "Contains %d words." count))
        count))))

(defun count-words-buffer-using-wc nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(defvar words-per-novel-page 250
  "A rough estimate of the number of words on an average page of
a novel. For use by \\[count-pages-buffer].")

(defvar words-per-academic-page 500
  "A rough estimate of the number of words on an average page of
an academic book. For use by \\[count-pages-buffer].")

(defun count-pages-buffer ()
  "Estimate the number of printed pages the current buffer would
will and print the result in the minibuffer."
  (interactive)
  (let* ((words (count-words-buffer))
         (novel-pages (/ words words-per-novel-page))
         (academic-pages (/ words words-per-academic-page)))
    (message "Roughly %d novel pages in length, or %d academic pages."
             novel-pages academic-pages)))

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

(defvar camdez/project-notes-file
  "notes.org"
  "Name or relative path from project root to notes file.")

;; TODO: create parent directories as needed. So I can store my notes
;; in `camdez-local/notes.org' if I want to.  Also test out overriding
;; this on a per-project basis.
;;
;; Maybe I should have a template that gets inserted / expanded for
;; the new notes file and / or a capture template that is local to the
;; project.
(defun camdez/find-project-notes ()
  "Open notes file for project."
  (interactive)
  (let ((project-dir (projectile-project-root)))
    (if project-dir
        (find-file (expand-file-name camdez/project-notes-file project-dir))
      (error "Not in a project!"))))

(defun camdez/Info-goto-from-command-help ()
  "Go to the Info node in the Emacs manual for the command
currently being viewed in `help-mode'."
  (interactive)
  (let ((help-function (car help-xref-stack-item))
        (help-symbol (cadr help-xref-stack-item)))
    (if (and (eq help-function 'describe-function)
             (commandp help-symbol))
        (Info-goto-emacs-command-node help-symbol)
      (error "Info is only available for commands"))))

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
          #'(lambda ()
              (define-key outline-mode-map "\C-j"
                'outline-newline-and-indent)))

(defun reread-config-file ()
  "Reread .emacs file."
  (interactive)
  (load-file user-init-file))

(defun camdez/show-buffer-file-name ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun camdez/touch ()
  "Run touch command on current file."
  (interactive)
  (when buffer-file-name
    (shell-command (concat "touch " (shell-quote-argument buffer-file-name)))
    (clear-visited-file-modtime)))

(defun camdez/kill-buffer-other-window ()
  "Kill the buffer currently displayed in the other window."
  (interactive)
  (other-window 1)
  (kill-buffer nil)
  (other-window -1))

;; Actually, it looks like there's a command that does this now, on
;; `C-x 4 0`.  `kill-buffer-and-window'.  Compare.
(defun camdez/delete-window (&optional p)
  "Kill current window.  If called with PREFIX, kill the buffer too."
  (interactive "P")
  (when p (kill-buffer nil))
  (delete-window))

(defun camdez/delete-other-windows (&optional p)
  "Make the selected window fill its frame.  If called with PREFIX,
kill all other visible buffers."
  (interactive "P")
  (when p
    (dolist (window (window-list))
      (unless (equal (window-buffer window) (current-buffer))
        (kill-buffer (window-buffer window)))))
  (delete-other-windows))

(defun buffer-file-basename (buffer)
  "Return the filename, with extension removed, of the file
associated with the buffer BUFFER.  Returns nil if there is no
file associated with BUFFER."
  (let ((filename (buffer-file-name buffer)))
    (if filename
        (file-name-nondirectory (file-name-sans-extension filename)))))

(defun buffers-same-file-basename (buffer)
  "Return the list of buffers which are associated with files
with the same basename (filename sans extension) as the file the
buffer BUFFER is associated with."
  (let ((basename (buffer-file-basename buffer)))
    (remove nil
            (mapcar #'(lambda (obuf)
                        (unless
                            (or (eq buffer obuf)
                                (not (string= basename (buffer-file-basename obuf))))
                          obuf))
                    (buffer-list)))))

(defun next-buffer-same-file-basename ()
  "Switch to the next buffer (if one exists) associated with a file of
the same basename (filename sans extension) as the file the current
buffer is associated with."
  (interactive)
  (let ((next-buffer (car (buffers-same-file-basename (current-buffer)))))
    (if next-buffer
        (switch-to-buffer next-buffer)
      (message "No buffers exist with same file basename"))))

(defun camdez/narrow-to-paragraph ()
  (interactive)
  (mark-paragraph)
  (narrow-to-region (point) (mark)))

;; TODO: trying using `clone-indirect-buffer' instead!
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

(defun camdez/toggle-chrome ()
  "Toggle display of graphical elements (tool bar, menu bar)."
  (interactive)
  (let ((state (if menu-bar-mode -1 +1)))
    (menu-bar-mode state)
    (tool-bar-mode state)))

;; Suck in entire identifier in search mode.  Name could possibly use
;; improvement.  If you steal one thing from my .emacs, make it this.
(defun camdez/isearch-yank-identifier ()
  (interactive)
  (isearch-yank-internal (lambda ()
                           (forward-sexp)
                           (point))))

(defun camdez/cycle (options val)
  "Returns the element following VAL in OPTIONS, or the first
element if VAL is not found."
  (interactive)
  (car
   (let ((tail (cdr (memq val options))))
     (if (consp tail)
         tail
       options))))

(defmacro setq-cycle (sym options)
  `(setq ,sym (camdez/cycle ,options ,sym)))

;; Useful when viewing old elisp files with hard tabs assumed to be 8
;; spaces wide.
(defun camdez/toggle-tab-width ()
  "Toggles `tab-width' between 8 and 2."
  (interactive)
  (setq-cycle tab-width '(2 8))
  (redraw-frame (selected-frame)) ; sometimes doesn't redisplay without input
  (message "Tab width set to %d." tab-width))

(defun camdez/toggle-show-paren-style ()
  "Toggle `show-paren-style' between `parenthesis' and `expression'."
  (interactive)
  (setq-cycle show-paren-style '(parenthesis expression)))

(defun camdez/lisp-insert-hr ()
  "Insert a horizontal rule comment."
  (interactive)
  (insert (make-string fill-column ?\;))
  (newline-and-indent))

(defun camdez/add-experiment ()
  "Add a new experiment to the experiments file."
  (interactive)
  (find-file camdez/experiments-file)
  (goto-char (point-min))
  (when (re-search-forward ";;; Code:" nil 'no-error)
    (insert "\n\n"))
  (camdez/lisp-insert-hr)
  (insert ";; ")
  (insert-date-and-time)
  (insert "\n\n"))

(defun camdez/switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

;; One quirk (not sure if I care) is that we don't get the scratch
;; buffer message inserted into the buffer if it's newly-created.
;; Also we're not setting the mode in the same way.
;;
;; Scratch gets its mode set in C code, but the basic algorithm is to
;; default to using `initial-major-mode' and fall back to the default
;; value of `major-mode' if that isn't set.
(defun camdez/switch-to-scratch (&optional p)
  (interactive "P")
  (switch-to-buffer
   (if p
       (with-current-buffer (generate-new-buffer "*scratch*")
         (funcall initial-major-mode)
         (current-buffer))
     "*scratch*")))

;;; STUFF MOSTLY BORROWED FROM OTHERS

;; Insert the date, the time, and the date & time at point.
(defvar insert-time-format "%T"
  "*Format for \\[insert-time] (c.f. `format-time-string' for how to format).")

(defvar insert-date-format "%Y-%m-%d"
  "*Format for \\[insert-date] (c.f. `format-time-string' for how to format).")

(defun insert-time ()
  "Insert the current time according to the variable `insert-time-format'."
  (interactive "*")
  (insert (format-time-string insert-time-format
                              (current-time))))

(defun insert-date ()
  "Insert the current date according to the variable `insert-date-format'."
  (interactive "*")
  (insert (format-time-string insert-date-format
                              (current-time))))

(defun insert-date-and-time ()
  "Insert the current date according to the variable `insert-date-format',
then a space, then the current time according to the variable
`insert-time-format'."
  (interactive "*")
  (progn
    (insert-date)
    (insert " ")
    (insert-time)))

(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (dabbrev-expand nil)
    (indent-for-tab-command)))

(defun underline-line ()
  "Underline the current line with `=', or underline the previous line
if at the beginning of a line."
  (interactive)
  (let ((cc (current-column)))
    (when (= cc 0)
      (forward-line -1)
      (end-of-line)
      (setq cc (current-column)))
    (newline)
    (dotimes (i cc)
      (insert "="))))

;; Prefer horizontal window splitting to vertical.
(defun split-window-sensibly (window)
  "Overwrite the normal version to prefer splitting
horizontally (side-by-side)."
  (or (and (window-splittable-p window t)
           ;; Split window horizontally.
           (with-selected-window window
             (split-window-right)))
      (and (window-splittable-p window)
           ;; Split window vertically.
           (with-selected-window window
             (split-window-below)))
      (and (eq window (frame-root-window (window-frame window)))
           (not (window-minibuffer-p window))
           ;; If WINDOW is the only window on its frame and is not the
           ;; minibuffer window, try to split it vertically disregarding
           ;; the value of `split-height-threshold'.
           (let ((split-height-threshold 0))
             (when (window-splittable-p window)
               (with-selected-window window
                 (split-window-below)))))))

;; From emacs-starter-kit
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;; commands.el ends here
