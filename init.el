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
(load-library "commands")               ; various utility commands
(load-library "skeletons")              ; skeletons (templates) and autoinsert settings
(load-library "config")                 ; general settings
(load-library "keys")                   ; keybindings
(load-library "experimental")

;;; PACKAGES

(defvar my-packages '(clojure-mode
                      coffee-mode
                      feature-mode
                      full-ack
                      gh
                      gist
                      haml-mode
                      ido-vertical-mode
                      magit
                      markdown-mode
                      nrepl
                      org
                      sass-mode
                      yaml-mode))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;; NON-MODE LIBRARIES

(add-to-list 'load-path (concat library-root "git-commit-mode"))
(require 'git-commit)

(require 'cl-lib)
(add-to-list 'load-path (concat library-root "diff-hl"))
(require 'diff-hl)

(add-to-list 'load-path (concat library-root "helm"))
(require 'helm-config)

(add-to-list 'load-path (concat library-root "projectile"))
(require 's)
(require 'dash)
(require 'projectile)
(projectile-global-mode)

(require 'ido-vertical-mode)
(ido-vertical-mode)

;; icicles - badass input completion
;(add-to-list 'load-path (concat library-root "icicles/"))
;(when (require 'icicles nil t)
;  (icy-mode t))

;; yasnippet - templated snippet insertion
(add-to-list 'load-path (concat library-root "yasnippet/"))
(when (require 'yasnippet nil t)
  ;(setq yas/root-directory (concat emacs-root "snippets/"))
  ;(yas/load-directory yas/root-directory)
  (setq yas/snippet-dirs (list (concat emacs-root "snippets/")))
  (yas/global-mode 1))

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
