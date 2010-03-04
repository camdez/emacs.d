;;; init.el - primary configuration file for Emacs (like a .emacs)
;;; Author: Cameron Desautels

;;; 3RD PARTY LIBRARIES

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/lib/")

;; icicles - badass input completion
(add-to-list 'load-path "~/.emacs.d/lib/icicles/")
(when (require 'icicles nil t)
  (icy-mode t))

;; muse-mode - publish to various formats
(when (require 'muse-mode nil t)
  (require 'muse-html)
  (require 'muse-latex)
  (require 'muse-project) ; publish files in projects

  (setq muse-project-alist
        '(("Work Notes" ("~/notes" :default "index")
           (:base "xhtml" :path "~/tmp/published_notes")
           (:base "pdf" :path "~/public_html/pdf"))))

  (setq muse-xhtml-style-sheet "<link type=\"text/css\" rel=\"stylesheet\" href=\"file:///home/cdesaute/notes/muse.css\" />"))

;; slime - the superior lisp interaction mode for emacs
(when (require 'slime nil t)
  (slime-setup))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.\\(mdown\\|markdown\\|markdn\\|md\\)\\'" . markdown-mode))


;;; STOCK LIBRARIES

(require 'autoinsert)


;;; MY COMMANDS

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


;;; SKELETONS

(define-skeleton blog-entry-skeleton
  "Inserts a PyBlosxom-style blog entry skeleton into the current buffer.
This only makes sense for empty buffers."
  "Title: "
  str | "Untitled Entry" \n
  "#postdate " (insert-date-and-time) \n
  "#tags " ("Enter a tag: " str ",") '(delete-backward-char 1) \n \n
  "<p>" _ "</p>")

(define-skeleton c-c++-header-skeleton
  "Inserts a skeleton for C and C++ header files in the format I like."
  (upcase
   (concat
    "__"
    (replace-regexp-in-string "\\." "_" (upcase (file-name-nondirectory buffer-file-name)))
    "__"))
  "#ifndef " str n "#define " str "\n\n" _ "\n\n#endif /*" str "*/")

(define-skeleton php-header-skeleton
  "Inserts a small PHP skeleton."
  "Description: "
  "<?php" \n \n
  "// " (file-name-nondirectory buffer-file-name) " - " str \n
  "//" \n
  "// Author: " user-full-name " <" user-mail-address ">" \n
  "// Date:   " (insert-date-and-time) \n
  "//" \n
  (copyright user-copyright-holder) \n \n
  _ \n \n
  "?>" \n
  )

(eval-after-load "autoinsert"
  '(progn
     (add-to-list 'auto-insert-alist '("\\.blog\\'" . blog-entry-skeleton))
     (add-to-list 'auto-insert-alist '("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . c-c++-header-skeleton))
     (add-to-list 'auto-insert-alist '("\\.php\\'" . php-header-skeleton))))


;;; OPTIONS AND CONFIGURATION

;; Only under Aquamacs Emacs
(when (boundp 'aquamacs-version)
  (set-background-color "black")
  (set-foreground-color "white")

  (setq mac-command-modifier 'meta)
  (setq x-select-enable-clipboard t)

  (setq mac-allow-anti-aliasing t)
  ;; To turn off antialiasing (looks like crap with current font):
  ;; 1. (setq mac-allow-anti-aliasing nil)
  ;; 2. `defaults write org.gnu.AquamacsEmacs AppleAntiAliasingThreshold 0`
  )


(setq user-full-name "Cameron Desautels")
(setq user-mail-address "camdez@gmail.com")
(setq user-copyright-holder "Cameron Desautels")           ;my creation

(setq inhibit-startup-message t)
(setq visible-bell t)
(blink-cursor-mode -1)                  ; make the bloody cursor stop blinking
(setq x-stretch-cursor t)               ; make the cursor wide over spaces, etc.
(setq-default indent-tabs-mode nil)     ; use spaces (not tabs) for indenting
(setq kill-ring-max 10)                 ; don't save too many kills (I don't use many)
(setq history-length 250)
(setq require-final-newline t)          ; always terminate last line in file
(setq default-major-mode 'text-mode)    ; default mode is text mode
(setq default-indicate-empty-lines t)   ; show which lines at the end of the buffer are blank via the gutter
(cua-mode 0)                            ; Aquamacs turns this crap on (messes with transient-mark-mode too)

(global-font-lock-mode t)               ; use syntax highlighting
(show-paren-mode t)                     ; highlight matching parentheses
(setq blink-matching-paren nil)         ; not needed since I'm highlighting the matches

(setq-default truncate-lines t)

(which-func-mode t)                     ; show current function in modeline

(setq frame-title-format "Emacs: %b %+%+ %f")
(setq ange-ftp-ftp-program-name "ftp")

;; This needs to be a mode-hook, in emacs22 it requires that we are in CC mode
;(c-set-style "bsd")                     ; set the c-indentation style to BSD style
(setq-default tab-width 2)              ; display tabs as being two spaces wide
(setq-default c-basic-offset 2)         ; in C-like modes, indent by two spaces

(mouse-wheel-mode 1)                    ; make the mouse wheel work
(auto-image-file-mode 1)                ; open images as images
(auto-compression-mode 1)               ; automagically explore compressed files when visited
(menu-bar-mode -1)                      ; hide the menu bar
(when window-system
  (tool-bar-mode -1)                    ; hide the tool bar
  (scroll-bar-mode -1))                 ; put the scroll bar on the right where it should be

;; desktop-save-mode - save the current state of work (disabled for now)
;; (setq desktop-dirname "/home/cdesaute/.emacs_desktop/")
;; (desktop-save-mode 1)
;; (add-to-list 'desktop-globals-to-save 'file-name-history)
;; (add-to-list 'desktop-modes-not-to-save 'dired-mode)
;; (add-to-list 'desktop-modes-not-to-save 'Info-mode)
;; (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;; (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Enable disabled operations
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Disable annoying operations
(put 'overwrite-mode 'disabled t)

;; open files in desirable modes based on file extensions
(add-to-list 'auto-mode-alist '("\\.uc\\'" . java-mode)) ; open UnrealScript files as java
(add-to-list 'auto-mode-alist '("\\.cs\\'" . java-mode))
(add-to-list 'auto-mode-alist '("mutt-.*-[0-9]+-[0-9]+-[0-9]+\\'" . post-mode))
(add-to-list 'auto-mode-alist '("\\.blog\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))


;; Be smart and don't display my passwords in shell mode...
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; Start text mode with auto-fill on
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Autoinsert text into empty files where defined
(add-hook 'find-file-hooks 'auto-insert)

;; Automagically turn on eldoc mode for emacs-lisp buffers
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; Don't insert spaces when inserting parentheses
(add-hook 'php-mode-hook '(lambda ()
                            (setq parens-require-spaces nil)))

;; Use awesome buffer switching mode
(if (> emacs-major-version 21)
    (iswitchb-mode 1)
  (iswitchb-default-keybindings))

;; Keep numbered backups
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(setq inferior-lisp-program "sbcl --noinform --no-linedit")


;;; KEYBINDINGS

(global-set-key [f2] 'goto-line)
(global-set-key [f3] (lambda ()
                       (interactive)
                       (switch-to-buffer (other-buffer))))
(global-set-key [f8] 'eshell)
(global-set-key [f9] 'speedbar)
; If running under X, have [f10] toggle display of menu-bar (and
; tool-bar) instead of running tmm-menubar.  Don't toggle one off and
; one on.
(if window-system
    (global-set-key [f10] (lambda ()
                            (interactive)
                            (when (y-or-n-p "Are you sure you want to toggle the chrome? ")
                              (if (eq menu-bar-mode tool-bar-mode)
                                  (progn
                                    (menu-bar-mode nil)
                                    (tool-bar-mode))
                                (menu-bar-mode nil))))))
(global-set-key [f11] 'compile)
(global-set-key [f12] 'recompile)
(global-set-key [del] 'delete-char)
(global-set-key "\C-x0" 'delete-window-replacement)
(global-set-key "\C-x1" 'delete-other-windows-replacement)
(global-set-key "\C-x4k" 'kill-buffer-other-window)
(global-set-key [C-right] 'next-buffer)
(global-set-key [C-left] 'prev-buffer)
(global-set-key [C-up] 'next-buffer-same-file-basename)
; This practially makes the previous line useless, but the differences
; should be investigated.
(global-set-key "\M-`" 'ff-find-other-file)
(global-set-key "\M-o" 'occur)

(global-set-key "\C-cf" 'auto-fill-mode)
(global-set-key "\C-cl" 'goto-line)
(global-set-key "\C-ch" 'hl-line-mode)

(setq html-mode-hook
      '(lambda ()
         (auto-fill-mode 1)
         (define-key html-mode-map "\C-c\C-p" 'php-mode)))
(setq php-mode-hook
      '(lambda ()
         (define-key php-mode-map "\C-c\C-p" 'html-mode)))
(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "\r" 'dired-find-alternate-file)))

(set-register ?e (cons 'file user-init-file))  ; quickly jump here with C-x r j e


;;; OTHER CRAP, GENERALLY WRITTEN BY OTHER PEOPLE

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


;;; EMACS SERVER STUFF

(server-start)

(add-hook 'server-switch-hook
          (lambda ()
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))

(add-hook 'server-done-hook
          (lambda ()
            (kill-buffer nil)
            (delete-frame)))


;;; GET THAT CUSTOMIZATION CRAP OUT OF HERE

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;;; SITE-SPECIFIC CODE
(load "local" t)                        ;load anything site-specific

;;; END
