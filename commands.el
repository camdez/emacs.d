;;; commands.el --- my utility commands

;; Copyright (C) 2010  Cameron Desautels
;; Author: Cameron Desautels <camdez@gmail.com>, others as noted

;; Escape all double-quotes (") in the region
(fset 'escape-region-double-quotes
   "\C-xnn\C-[<\C-[%\"\C-m\\\"\C-m!\C-[>\C-xnw")

(defalias 'elisp-mode 'emacs-lisp-mode)
(defalias 'cleanup-whitespace 'whitespace-cleanup)

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
          '(lambda ()
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

;; TODO tool-bar-mode only exists under a window-system
(defun toggle-chrome ()
  "Toggle display of graphical elements (tool bar, menu bar)."
  (interactive)
    (if (eq menu-bar-mode tool-bar-mode)
        (progn
          (menu-bar-mode nil)
          (tool-bar-mode nil))
      (tool-bar-mode nil)))

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
      (previous-line 1)
      (end-of-line)
      (setq cc (current-column)))
    (newline)
    (dotimes (i cc)
      (insert "="))))

;; Suck in entire identifier in search mode.  Name could possibly use
;; improvement.  If you steal one thing from my .emacs, make it this.
(defun camdez/isearch-yank-identifier ()
  (interactive)
  (isearch-yank-internal (lambda ()
                           (forward-sexp)
                           (point))))

;; Useful when viewing old elisp files with hard tabs assumed to be 8
;; spaces wide.
(defun camdez/toggle-tab-width ()
  "Toggles `tab-width' between 8 and 2."
  (interactive)
  (setq tab-width
        (if (= tab-width 2)
            8
          2))
  (redraw-frame (selected-frame)) ; sometimes doesn't redisplay without input
  (message "Tab width set to %d." tab-width))

(defun camdez/toggle-show-paren-style ()
  "Toggle `show-paren-style' between `parenthesis' and `expression'."
  (interactive)
  (setq show-paren-style
        (if (eq show-paren-style 'parenthesis)
            'expression
          'parenthesis)))

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
