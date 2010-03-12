;;; config.el --- various general settings
;;; Author: Cameron Desautels <camdez@gmail.com>

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
(transient-mark-mode 0)                 ; no way, dude
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
(unless (featurep 'ns)
  (menu-bar-mode -1))                   ; hide the menu bar
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

;; Be smart and don't display my passwords in shell mode...
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; Start text mode with auto-fill on
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Use awesome buffer switching mode
(if (> emacs-major-version 21)
    (iswitchb-mode 1)
  (iswitchb-default-keybindings))

;; Keep numbered backups
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;; Only under Aquamacs Emacs
(when (featurep 'aquamacs)
  (setq mac-command-modifier 'meta)
  (setq mac-allow-anti-aliasing t)
  (setq x-select-enable-clipboard t))

;; Only under Cocoa Emacs
(when (featurep 'ns)
  (setq ns-command-modifier 'meta))

;;; config.el ends here
