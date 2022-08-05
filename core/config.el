;;; config.el --- various general settings
;;; Author: Cameron Desautels <camdez@gmail.com>

(setq user-full-name "Cameron Desautels"
      user-mail-address "camdez@gmail.com"
      user-copyright-holder "Cameron Desautels" ; my creation

      history-length 250

      inhibit-startup-message t
      require-final-newline t                    ; always terminate last line in file
      view-read-only t                           ; use handy mode instead of just beeping all the time
      vc-follow-symlinks t
      apropos-do-all t                           ; search all the things

      set-mark-command-repeat-pop t              ; after `C-u C-SPC`, keep popping with `C-SPC`
      visible-bell t
      x-stretch-cursor t                         ; make the cursor wide over tabs, etc.
      blink-matching-paren nil                   ; not needed since I'm highlighting the matches
      frame-title-format "Emacs: %b %+%+ %f"
      echo-keystrokes 0.02
      isearch-allow-scroll t                     ; scrolling shouldn't cancel search
      isearch-lazy-count t                       ; display count of search results in minibuffer
      scroll-preserve-screen-position t          ; when scrolling, keep point in the same position on screen

      backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      make-backup-files t
      backup-by-copying t
      version-control t                          ; keep numbered back-ups
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      delete-by-moving-to-trash t

      dired-kill-when-opening-new-dired-buffer t ; only one dired buffer when navigating
      dired-listing-switches "-alh"              ; use human-friendly file sizes

      eldoc-idle-delay 0)

(setq whitespace-style
      '(face
        tabs
        spaces
        trailing
        space-before-tab
        indentation
        empty
        space-mark
        tab-mark))

(setq-default major-mode 'text-mode             ; default mode is text mode
              tab-width 2                       ; display tabs as being two spaces wide
              indent-tabs-mode nil              ; use spaces (not tabs) for indenting
              indicate-empty-lines t
              truncate-lines t)                 ; don't visually wrap long lines

;; Only under Aquamacs Emacs
(when (featurep 'aquamacs)
  (setq mac-command-modifier 'meta
        mac-allow-anti-aliasing t
        select-enable-clipboard t))

;; Only under Cocoa Emacs
(when (featurep 'ns)
  (setq ns-command-modifier 'meta
        trash-directory "~/.Trash"
        delete-by-moving-to-trash t
        dired-use-ls-dired nil))

;; Only under the Mac port
(when (featurep 'mac)
  (setq mac-option-modifier 'meta))

(blink-cursor-mode       -1)     ; make the bloody cursor stop blinking
(transient-mark-mode     -1)     ; no way, dude
(cua-mode                -1)     ; Aquamacs turns this crap on (messes with transient-mark-mode too)
(column-number-mode      +1)
(global-hl-line-mode     +1)
(recentf-mode            +1)     ; track recently-opened files
(show-paren-mode         +1)     ; highlight matching parentheses
(ido-mode                +1)     ; use awesome buffer switching mode
(winner-mode             +1)     ; remember window configurations

(if window-system
    (progn
      (tool-bar-mode -1)         ; hide the tool bar
      (scroll-bar-mode -1))      ; hide the scroll bar
  (menu-bar-mode -1))            ; hide the menu bar

(add-hook 'before-save-hook 'delete-trailing-whitespace) ; maybe only in `prog-mode`?
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Enable disabled operations
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Disable annoying operations
(put 'overwrite-mode 'disabled t)

;; Be smart and don't display my passwords in shell mode...
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; Advice
(defadvice kill-some-buffers (around kill-some-buffers-y-or-n first)
  "When running `kill-some-buffers' use 'y' and 'n' for response,
regardless of if 'yes' or 'no' are generally preferred."
  (cl-flet ((yes-or-no-p (args) (y-or-n-p args)))
    ad-do-it))
(ad-activate 'kill-some-buffers)

;; This is kind of annoying when killing multiple forms in succession
;; and then yanking them back.  It removes the newlines.  Maybe we can
;; notice that case and do things right?  Or kill at least one newline
;; (rather than deleting).
(defun camdez/kill-sexp--delete-blank-line (&optional arg interactive)
  "If killing sexp leaves line blank, delete line too."
  (when (and interactive
             (looking-at-p "^$")
             (not (eobp)))
    (delete-char 1)))
(advice-add 'kill-sexp :after #'camdez/kill-sexp--delete-blank-line)

;; Don't display (lengthy) minor mode information in `describe-mode'.
(defun camdez/hide-minor-modes (orig-fn &rest args)
  "Make it look like we don't have any minor modes."
  (let ((minor-mode-alist nil)
        (minor-mode-list nil))
    (apply orig-fn args)))
(advice-add 'describe-mode :around #'camdez/hide-minor-modes)

;;; config.el ends here
