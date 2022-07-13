;;; init.el - primary configuration file for Emacs (like a .emacs)
;;; Author: Cameron Desautels

(setq gc-cons-threshold 64000000)

;; `benchmark-init/show-durations-tabulated'
(require 'benchmark-init nil t)

;;;

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
    ag
    checkbox
    cider
    clj-refactor
    clojure-mode
    company
    diminish
    feature-mode
    flycheck
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
    notmuch
    org
    paredit
    page-break-lines
    projectile
    rainbow-mode
    rspec-mode
    sass-mode
    unfill
    unicode-fonts
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

(require 'cl-lib)

(defun camdez/install-packages ()
  "Ensure the packages I use are installed.  See `camdez/packages'."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p camdez/packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))

(camdez/install-packages)

(eval-when-compile
  (require 'use-package))

;;; CORE

(load-library "theme")                  ; make things look pretty
(load-library "modes")                  ; modes for editing various types of files
(load-library "org-settings")           ; org-mode configuration
(load-library "commands")               ; various utility commands
(load-library "skeletons")              ; skeletons (templates) and autoinsert settings
(load-library "config")                 ; general settings
(load-library "keys")                   ; keybindings

;;; HACKS + EXPERIMENTS

;; Load experiments, preferring byte-compiled version
(load (expand-file-name "experimental" camdez/core-dir) 'no-error)

;;; NON-MAJOR-MODE LIBRARIES

(when (require 'company nil t)
  ;; TODO: take a look at `helm-company` and see if that's
  ;; interesting.
  (setq company-tooltip-align-annotations t
        company-require-match nil)
  ;; Not needed.  Pops up after a brief delay.  If we're going to have
  ;; this, it needs to be disabled in the minibuffer where it fucks
  ;; shit up.
  ;;(global-set-key (kbd "<tab>") 'company-indent-or-complete-common)
  ;;(global-set-key (kbd "TAB") 'company-indent-or-complete-common) ; terminal
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle) ; terminal
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (global-company-mode))

(when (require 'ido-vertical-mode nil t)
  (ido-vertical-mode))

;; ace-jump - get there faster
(require 'ace-jump-mode nil t)

;; which-key - display available keybindings
(when (require 'which-key nil t)
  (which-key-mode 1))

;; keyfreq - command use statistics (`keyfreq-show' to see stats)
(when (require 'keyfreq nil t)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Helm - incremental completion and selection narrowing framework
(when (require 'helm-config nil t)
  (require 'helm-mode) ; getting errors without this
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "M-g i") 'helm-imenu)
  (global-set-key (kbd "M-o") 'helm-occur)
  (global-set-key (kbd "C-x r l") 'helm-bookmarks))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; page-break-lines - display page separator characters (^L) as lines
(when (require 'page-break-lines nil t)
  (setq page-break-lines-max-width 80)
  (add-to-list 'page-break-lines-modes 'prog-mode)
  (global-page-break-lines-mode +1))

;; projectile - project interaction
(when (require 'projectile nil t)
  (projectile-mode)
  ;; Temporary change since I've dropped back to the stable version of
  ;; projectile-mode which doesn't have this binding yet:
  (define-key projectile-command-map (kbd "f") 'helm-projectile)
  (setq projectile-enable-caching t
        projectile-switch-project-action 'projectile-dired))

;; diminish - hide extraneous mode indicators
(when (require 'diminish nil t)
  (eval-after-load 'auto-complete       '(diminish 'auto-complete-mode))
  (eval-after-load 'eldoc               '(diminish 'eldoc-mode))
  (eval-after-load 'page-break-lines    '(diminish 'page-break-lines-mode))
  (eval-after-load 'projectile          '(diminish 'projectile-mode))
  (eval-after-load 'volatile-highlights '(diminish 'volatile-highlights-mode))
  (eval-after-load 'which-key           '(diminish 'which-key-mode))
  (eval-after-load 'yasnippet           '(diminish 'yas-minor-mode)))

;; yasnippet - templated snippet insertion
(when (require 'yasnippet nil t)
  (setq yas-snippet-dirs (list (expand-file-name "snippets" camdez/emacs-dir))
        yas-verbosity 2)                    ; don't show messages at init time
  (yas-global-mode 1))

;; midnight - clean up stale buffers (see `clean-buffer-list')
(when (require 'midnight nil t)
  (midnight-delay-set 'midnight-delay 0)    ; run at midnight
  (setq clean-buffer-list-delay-general 1)) ; kill buffers after one day

;; imenu
(setq imenu-auto-rescan t)

;; unicode-fonts (emoji!)
(when (require 'unicode-fonts)
  (unicode-fonts-setup))

;; zoom - automatically balance window sizes
(use-package zoom
  :diminish
  :config
  (zoom-mode 1)
  :custom
  (zoom-size '(0.618 . 0.618)))

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
