;;; init.el - primary configuration file for Emacs (like a .emacs)  -*- lexical-binding: t; -*-
;;; Author: Cameron Desautels

;; `benchmark-init/show-durations-tabulated'
(require 'benchmark-init nil t)

;;;

(defvar camdez/emacs-dir
  (file-name-directory (or load-file-name
                           buffer-file-name))
  "The root directory of my Emacs configuration.")

(defvar camdez/core-dir (expand-file-name "core" camdez/emacs-dir)
  "Directory containing core configuration files.")

(defvar camdez/core-modules
  '("theme"            ; make things look pretty
    "modes"            ; modes for editing various types of files
    "org-settings"     ; org-mode configuration
    "commands"         ; various utility commands
    "skeletons"        ; skeletons (templates) and autoinsert settings
    "config"           ; general settings
    "keys")            ; keybindings
  "Core configuration files to load, in load order.  Paths are
relative to `camdez/core-dir'.  Excludes file extensions so
compiled versions can be preferred, where present.")

(defvar camdez/experiments-file (expand-file-name "experimental.el" camdez/core-dir)
  "File containing my latest elisp experiments.")

;;; PACKAGES

(defvar camdez/packages
  '(ace-jump-mode
    ag
    checkbox
    cider
    clj-refactor
    clojure-mode
    company
    deft
    diminish
    dired-collapse
    dockerfile-mode
    feature-mode
    flycheck
    flycheck-clj-kondo
    flycheck-color-mode-line
    helm
    helm-projectile
    highlight-symbol
    htmlize           ; highlight code in org exports
    ido-vertical-mode
    keyfreq           ; command use statistics
    magit
    marginalia
    markdown-mode
    monokai-theme
    no-littering
    notmuch
    org
    org-cliplink
    paredit
    page-break-lines
    projectile
    rainbow-delimiters
    rainbow-mode
    rspec-mode
    sass-mode
    unfill
    use-package
    which-key
    yaml-mode
    yasnippet
    zoom))

(require 'package)
(setq package-archives
      '(("gnu"            . "http://elpa.gnu.org/packages/")
        ("melpa-unstable" . "http://melpa.org/packages/")))
(package-initialize)

(defun camdez/install-packages ()
  "Ensure the packages I use are installed.  See `camdez/packages'."
  (interactive)
  (let ((missing-packages (seq-remove #'package-installed-p camdez/packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))

(camdez/install-packages)

;;;

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)

;;;

(use-package no-littering)              ; needs to happen early

;;; CORE

(dolist (module camdez/core-modules)
  (load (expand-file-name module camdez/core-dir)))

;;; HACKS + EXPERIMENTS

;; Load experiments, preferring byte-compiled version
(load (expand-file-name "experimental" camdez/core-dir) 'no-error)

;;; NON-MAJOR-MODE LIBRARIES

;; ace-jump - get there faster
(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

;; company - fancy in-buffer completion
(use-package company
  :demand t
  :bind (:map company-active-map
              (("<tab>" . company-complete-common-or-cycle)
               ("TAB" . company-complete-common-or-cycle) ; terminal
               ("<backtab>" . company-select-previous)))
  :config
  (setq company-tooltip-align-annotations t
        company-require-match nil)

  ;; Fix company changing case of text when trying to autocomplete
  ;; code.
  (with-eval-after-load 'company-dabbrev-code
    (add-to-list 'company-dabbrev-code-modes 'yaml-mode))

  ;; Not needed.  Pops up after a brief delay.  If we're going to have
  ;; this, it needs to be disabled in the minibuffer where it fucks
  ;; shit up.
  ;;(global-set-key (kbd "<tab>") 'company-indent-or-complete-common)
  ;;(global-set-key (kbd "TAB") 'company-indent-or-complete-common) ; terminal
  (global-company-mode))

;; dashboard - makes a nice initial buffer with jumping off points
(use-package dashboard
  :config
  (setq dashboard-projects-backend 'projectile
        dashboard-items '((recents . 5)
                          (projects . 5)
                          (bookmarks . 5)
                          ;;(agenda . 20) ; too slow!
                          )
        dashboard-set-footer nil
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-startup-banner 'logo
        dashboard-week-agenda nil)
  (add-to-list 'dashboard-mode-hook
               (lambda ()
                 (setq indicate-empty-lines nil)))
  (dashboard-setup-startup-hook))

;; eldoc - show function arguments in minibuffer
(use-package eldoc
  :diminish eldoc-mode
  :defer
  :config
  (setq eldoc-idle-delay 0))

;; keyfreq - command use statistics (`keyfreq-show' to see stats)
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Helm - incremental completion and selection narrowing framework
(use-package helm
  :demand t
  :bind (("C-c y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x r l" . helm-bookmarks)
         ("M-g i" . helm-imenu)
         ("M-o" . helm-occur)
         ("M-x" . helm-M-x)))

;; ido-vertical-mode - display ido completion options vertically
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode))

;; magit - the greatest git client known to man
(use-package magit
  :bind ("C-c m" . magit-status))

;; Marginalia - add marginalia to minibuffer completions
(use-package marginalia
  :demand t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

;; midnight - kill stale buffers automagically (see `clean-buffer-list')
(use-package midnight
  :defer 30
  :config
  (midnight-delay-set 'midnight-delay 0)    ; run at midnight
  (setq clean-buffer-list-delay-general 1)) ; kill buffers after one day

;; page-break-lines - display page separator characters (^L) as lines
(use-package page-break-lines
  :diminish
  :config
  (setq page-break-lines-max-width 80)
  (add-to-list 'page-break-lines-modes 'prog-mode)
  (global-page-break-lines-mode))

;; projectile - project interaction
(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :diminish
  :config
  (setq projectile-completion-system 'helm
        projectile-create-missing-test-files t
        projectile-enable-caching t
        projectile-switch-project-action 'projectile-dired)
  (projectile-mode))

;; recentf - track recently opened files (built-in)
(use-package recentf
  :config
  (setq recentf-max-saved-items 50)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (recentf-mode))

;; which-key - display available keybindings
(use-package which-key
  :diminish
  :config
  (which-key-mode))

;; yasnippet - templated snippet insertion
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" camdez/emacs-dir))
        yas-verbosity 2)            ; don't show messages at init time
  (yas-global-mode))

;; zoom - automatically balance window sizes
(use-package zoom
  :diminish
  :custom
  (zoom-size '(0.618 . 0.618)))

;;; EMACS SERVER

;;(server-start)

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

(load (expand-file-name "local" camdez/core-dir) 'no-error)

;;; GARBAGE COLLECTION

(setq gc-cons-threshold 800000) ; restore from value set in `early-init-file'

;;; END
