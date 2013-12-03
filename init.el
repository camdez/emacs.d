;;; init.el - primary configuration file for Emacs (like a .emacs)
;;; Author: Cameron Desautels

(defvar emacs-root
  (file-name-directory (or load-file-name
                           buffer-file-name)))
(defvar library-root (concat emacs-root "lib/"))

(add-to-list 'load-path emacs-root)
(add-to-list 'load-path library-root)

;;; PACKAGES

(defvar my-packages '(ace-jump-mode
                      clojure-mode
                      coffee-mode
                      color-theme
                      deft
                      feature-mode
                      find-file-in-project
                      full-ack
                      gh
                      gist
                      git-commit-mode
                      haml-mode
                      ido-at-point
                      ido-vertical-mode
                      idomenu
                      magit
                      markdown-mode
                      muse
                      nrepl
                      org
                      sass-mode
                      visible-mark
                      yaml-mode
                      yasnippet))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;; ROCK & ROLL

(load-library "theme")                  ; make things look pretty
(load-library "modes")                  ; modes for editing various types of files
(load-library "commands")               ; various utility commands
(load-library "skeletons")              ; skeletons (templates) and autoinsert settings
(load-library "config")                 ; general settings
(load-library "keys")                   ; keybindings
(load-library "experimental")

;;; NON-MODE LIBRARIES

(ido-vertical-mode)
(ido-at-point-mode)

;; yasnippet - templated snippet insertion
(when (require 'yasnippet nil t)
  (setq yas-snippet-dirs (list (concat emacs-root "snippets/")))
  (yas/global-mode 1))

;; imenu
(setq imenu-auto-rescan t)

;;; EMACS SERVER

;(server-start)

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
