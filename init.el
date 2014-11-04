;;; init.el - primary configuration file for Emacs (like a .emacs)
;;; Author: Cameron Desautels

(defvar camdez/emacs-dir
  (file-name-directory (or load-file-name
                           buffer-file-name))
  "The root directory of my Emacs configuration.")
(defvar camdez/core-dir (expand-file-name "core" camdez/emacs-dir)
  "Directory containing core configuration files.")

(add-to-list 'load-path camdez/core-dir)

;;; PACKAGES

(defvar camdez/my-packages
  '(ace-jump-mode
    cider
    clojure-mode
    coffee-mode
    color-theme
    feature-mode
    full-ack
    gh
    gist
    git-commit-mode
    guide-key
    haml-mode
    ido-at-point
    ido-vertical-mode
    idomenu
    magit
    markdown-mode
    org
    projectile
    sass-mode
    smex              ; ido for M-x
    visible-mark
    yaml-mode
    yasnippet))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(dolist (pkg camdez/my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;; CORE

(load-library "theme")                  ; make things look pretty
(load-library "modes")                  ; modes for editing various types of files
(load-library "commands")               ; various utility commands
(load-library "skeletons")              ; skeletons (templates) and autoinsert settings
(load-library "config")                 ; general settings
(load-library "keys")                   ; keybindings
(load-library "experimental")

;;; NON-MAJOR-MODE LIBRARIES

(ido-vertical-mode)
(ido-at-point-mode)

;; ace-jump - get there faster
(when (require 'ace-jump-mode nil t)
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

;; guide-key - handy visual reference for keychains
(when (require 'guide-key nil t)
  (setq guide-key/guide-key-sequence '("C-x r" "C-x v" "C-x 4" "C-x 5" "C-x RET"
                                       "C-x C-k" "C-c p" "M-s h" "C-h"))
  (guide-key-mode 1))

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
(load custom-file)

;;; SITE-SPECIFIC CODE

(load "local" t)

;;; END
