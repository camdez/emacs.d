;;; init.el - primary configuration file for Emacs (like a .emacs)
;;; Author: Cameron Desautels

(defvar emacs-root
  (file-name-directory (or load-file-name
                           buffer-file-name)))
(defvar library-root (concat emacs-root "lib/"))

(add-to-list 'load-path emacs-root)
(add-to-list 'load-path library-root)

;;; ROCK & ROLL

(load-library "theme")                  ; make things look pretty
(load-library "modes")                  ; modes for editing various types of files
(load-library "utilities")              ; various utility functions
(load-library "skeletons")              ; skeletons (templates) and autoinsert settings
(load-library "config")                 ; general settings
(load-library "keys")                   ; keybindings

;;; NON-MODE LIBRARIES

;; icicles - badass input completion
(add-to-list 'load-path (concat library-root "icicles/"))
(when (require 'icicles nil t)
  (icy-mode t))

;;; EMACS SERVER

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

;;; CUSTOMIZATION SYSTEM

(setq custom-file (concat emacs-root "custom.el"))
(load custom-file)

;;; SITE-SPECIFIC CODE

(load "local" t)

;;; END
