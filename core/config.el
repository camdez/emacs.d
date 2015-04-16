;;; config.el --- various general settings
;;; Author: Cameron Desautels <camdez@gmail.com>

(setq user-full-name "Cameron Desautels"
      user-mail-address "camdez@gmail.com"
      user-copyright-holder "Cameron Desautels" ; my creation

      kill-ring-max 20                          ; don't save too many kills
      history-length 250

      inhibit-startup-message t
      default-major-mode 'text-mode             ; default mode is text mode
      require-final-newline t                   ; always terminate last line in file
      view-read-only t                          ; use handy mode instead of just beeping all the time
      vc-follow-symlinks t

      visible-bell t
      x-stretch-cursor t                        ; make the cursor wide over tabs, etc.
      blink-matching-paren nil                  ; not needed since I'm highlighting the matches
      frame-title-format "Emacs: %b %+%+ %f"
      echo-keystrokes 0.02
      isearch-allow-scroll t                    ; scrolling shouldn't cancel search
      scroll-preserve-screen-position t         ; when scrolling, keep point in the same position on screen
      default-indicate-empty-lines t

      version-control t                         ; keep numbered back-ups
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

(setq-default tab-width 2                       ; display tabs as being two spaces wide
              indent-tabs-mode nil              ; use spaces (not tabs) for indenting
              show-trailing-whitespace t
              truncate-lines t)                 ; don't visually wrap long lines

;; Only under Aquamacs Emacs
(when (featurep 'aquamacs)
  (setq mac-command-modifier 'meta
        mac-allow-anti-aliasing t
        x-select-enable-clipboard t))

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
(global-font-lock-mode   +1)     ; use syntax highlighting
(column-number-mode      +1)
(show-paren-mode         +1)     ; highlight matching parentheses
(global-auto-revert-mode +1)     ; automatically reload files which are externally modified
(auto-image-file-mode    +1)     ; open images as images
(auto-compression-mode   +1)     ; automagically explore compressed files when visited
(ido-mode                +1)     ; use awesome buffer switching mode
(winner-mode             +1)

(if window-system
    (progn
      (tool-bar-mode -1)         ; hide the tool bar
      (scroll-bar-mode -1))      ; put the scroll bar on the right
  (menu-bar-mode -1))            ; hide the menu bar

(add-hook 'before-save-hook 'delete-trailing-whitespace)
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
  (flet ((yes-or-no-p (args) (y-or-n-p args)))
    ad-do-it))
(ad-activate 'kill-some-buffers)

(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

;; Make `backward-up-list' still succeed when point is inside a
;; string.
;;
;; Lots of thanks to:
;; http://stackoverflow.com/questions/18921320/emacs-bulletproof-up-list
(defadvice backward-up-list (before dont-choke-on-strings
                                    (&optional arg))
  (let* ((s (syntax-ppss))
         (in-list (nth 3 s))
         (str-or-com-beg (nth 8 s)))
    (when in-list
      (goto-char str-or-com-beg))))
(ad-activate 'backward-up-list)

;;; config.el ends here
