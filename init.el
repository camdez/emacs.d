;;; init.el - primary configuration file for Emacs (like a .emacs)
;;; Author: Cameron Desautels

(defvar camdez/emacs-dir
  (file-name-directory (or load-file-name
                           buffer-file-name))
  "The root directory of my Emacs configuration.")
(defvar camdez/core-dir (expand-file-name "core" camdez/emacs-dir)
  "Directory containing core configuration files.")

(add-to-list 'load-path camdez/core-dir)

(defvar camdez/experiments-file (expand-file-name "experimental.el" camdez/core-dir)
  "File containing my latest elisp experiments.")

;;; PACKAGES

(defvar camdez/packages
  '(ace-jump-mode
    ack-and-a-half
    auto-complete
    cider
    clojure-mode
    coffee-mode
    diminish
    feature-mode
    flycheck
    full-ack
    gh
    gist
    git-commit-mode
    guide-key
    haml-mode
    ido-vertical-mode
    idomenu           ; ido for `imenu'
    keyfreq           ; command use statistics
    magit
    markdown-mode
    monokai-theme
    org
    paredit
    projectile
    rspec-mode
    sass-mode
    scala-mode2
    smex              ; ido for `M-x'
    unfill
    yaml-mode
    yasnippet))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
             t)
(package-initialize)

(require 'cl-lib)

(defun camdez/install-packages ()
  "Ensure the packages I use are installed. See `camdez/packages'."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p camdez/packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))

(camdez/install-packages)

;;; CORE

(load-library "theme")                  ; make things look pretty
(load-library "modes")                  ; modes for editing various types of files
(load-library "commands")               ; various utility commands
(load-library "skeletons")              ; skeletons (templates) and autoinsert settings
(load-library "config")                 ; general settings
(load-library "keys")                   ; keybindings

;;; HACKS + EXPERIMENTS

(load camdez/experiments-file 'no-error)

;;; NON-MAJOR-MODE LIBRARIES

(when (require 'ido-vertical-mode nil t)
  (ido-vertical-mode))

(when (require 'ido-at-point nil t)
  (ido-at-point-mode))

(require 'smex nil t)

;; ace-jump - get there faster
(require 'ace-jump-mode nil t)

;; guide-key - handy visual reference for keychains
(when (require 'guide-key nil t)
  (setq guide-key/guide-key-sequence '("C-x r" "C-x v" "C-x 4" "C-x 5" "C-x RET"
                                       "C-x C-k" "C-c p" "M-s h" "C-h"))
  (guide-key-mode 1))

;; keyfreq - command use statistics (`keyfreq-show' to see stats)
(when (require 'keyfreq nil t)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; projectile - project interaction
(when (require 'projectile nil t)
  (projectile-global-mode)
  (setq projectile-enable-caching t
        projectile-switch-project-action 'projectile-dired))

;; yasnippet - templated snippet insertion
(when (require 'yasnippet nil t)
  (setq yas-snippet-dirs (list (expand-file-name "snippets" camdez/emacs-dir)))
  (yas/global-mode 1))

;; midnight - clean up stale buffers (see `clean-buffer-list')
(when (require 'midnight nil t)
  (midnight-delay-set 'midnight-delay 0)    ; run at midnight
  (setq clean-buffer-list-delay-general 1)) ; kill buffers after one day

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

;;; NONSENSE

;; Unbelievably this has to be here because of the bullshit lengths
;; `display-startup-echo-area-message' goes to to verify it.  If it
;; doesn't get patched soon I'm just going to override the method.
(setq inhibit-startup-echo-area-message "camdez")

;;; CUSTOMIZATION SYSTEM

(setq custom-file (expand-file-name "custom.el" camdez/core-dir))
(load custom-file 'no-error)

;;; SITE-SPECIFIC CODE

(load "local" 'no-error)

;;; END
