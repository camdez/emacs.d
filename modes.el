;;; modes.el --- load modes and tweaks for various file types
;;; Author: Cameron Desautels <camdez@gmail.com>

;; dired mode (+ dired-x)
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.
            (setq dired-omit-files
                  (concat dired-omit-files "\\|.DS_Store$"))))
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))

;; help-mode
(eval-after-load 'help-mode
  '(define-key help-mode-map "l" 'help-go-back)) ; consistent with info-mode

;; info-mode
(defun camdez/Info-mode-hook ()
  (interactive)
  (setq show-trailing-whitespace nil))

(add-hook 'Info-mode-hook 'camdez/Info-mode-hook)

;; text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; sh-mode
(eval-after-load 'sh-script
  '(setq sh-basic-offset 2))

;; cc-mode
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 2)))

;; magit-mode
(add-to-list 'load-path (concat library-root "magit/"))
(autoload 'magit-status "magit"
  "Mode for working with git." t)

;; full-ack
(add-to-list 'load-path (concat library-root "full-ack/"))
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; muse-mode - publish to various formats
(add-to-list 'load-path (concat library-root "muse-mode/"))
(when (require 'muse-mode nil t)
  (require 'muse-html)
  ;;  (require 'muse-latex)
  (require 'muse-project) ; publish files in projects

  (setq muse-project-alist
        '(("Personal Wiki" ("~/Writings/notes" :default "index")
           (:base "xhtml" :path "~/tmp/html_notes")
          ;(:base "pdf"   :path "~/tmp/pdf_notes")
           ))))

;; slime - the superior lisp interaction mode for emacs
(when (require 'slime nil t)
  (slime-setup))

(setq inferior-lisp-program "sbcl --noinform --no-linedit")

;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; shell-mode
(add-hook 'shell-mode-hook (lambda ()
                             (setq tab-width 8)))

;; rst-mode
(add-hook 'rst-mode-hook 'turn-on-visual-line-mode)

;; ruby-mode
(let ((mode-files '("Gemfile" "Rakefile" "Capfile" "Guardfile" "config.ru"))
      (mode-extns '("rake" "gemspec" "jbuilder" "builder" "prawn")))
  (add-to-list 'auto-mode-alist (cons (concat       (regexp-opt mode-files) "\\'") 'ruby-mode))
  (add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt mode-extns) "\\'") 'ruby-mode)))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (define-key ruby-mode-map "\C-x\C-t" 'transpose-lines)))

;; rinari
(when (require 'ruby-mode nil t)
  (add-to-list 'load-path (concat library-root "rinari"))
  (require 'rinari nil t))

;; feature-mode for Cucumber
(add-to-list 'load-path (concat library-root "feature-mode"))
(require 'feature-mode)

;; markdown-mode
(add-to-list 'load-path (concat library-root "markdown-mode/"))
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.\\(mdown\\|markdown\\|markdn\\|md\\)\\'" . markdown-mode))

(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map (kbd "<tab>") 'yas/expand))

;; haml-mode
(add-to-list 'load-path (concat library-root "haml-mode/"))
(autoload 'haml-mode "haml-mode"
  "Major mode for editing HAML files" t)
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

;; sass-mode
(add-to-list 'load-path (concat library-root "sass-mode/"))
(autoload 'sass-mode "sass-mode"
  "Major mode for editing SASS files" t)
(add-to-list 'auto-mode-alist '("\\.\\(sass\\|scss\\)\\'" . sass-mode))

;; css-mode
(eval-after-load 'css-mode
  '(setq css-indent-offset 2))

;; js-mode
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?\\'" . js-mode))

(eval-after-load 'js
  '(setq js-indent-level 2))

;; coffee-mode
(add-to-list 'load-path (concat library-root "coffee-mode"))
(autoload 'coffee-mode "coffee-mode"
  "Major mode for editing CoffeeScript.")
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile\\'" . coffee-mode))

;; php-mode
(autoload 'php-mode "php-mode"
  "Major mode for editing PHP code.")
(add-to-list 'auto-mode-alist '("\\.php[s34]?\\'" . php-mode))

(add-hook 'php-mode-hook
          '(lambda ()
             (set (make-local-variable 'parens-require-spaces) nil))) ; Don't insert spaces when inserting parentheses

(eval-after-load 'php-mode
  '(define-key php-mode-map "\C-c\C-p" 'html-mode))

;; clojure-mode
(add-to-list 'load-path (concat library-root "clojure-mode"))
(autoload 'clojure-mode "clojure-mode"
  "Major mode for editing Clojure.")
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

;; html-mode
(add-to-list 'auto-mode-alist '("\\.blog\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml\\'" . html-mode))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map "\C-c\C-p" 'php-mode))

;; yaml-mode
(add-to-list 'load-path (concat library-root "yaml-mode/"))
(autoload 'yaml-mode "yaml-mode"
  "Major mode for editing files in the YAML data serialization format" t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(eval-after-load 'yaml-mode
  '(define-key yaml-mode-map "\C-m" 'newline-and-indent))

;; conf-mode
(add-to-list 'auto-mode-alist '("Procfile\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("Gemfile.lock\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.git\\(config\\|modules\\)\\'" . conf-mode))

;; snippet-mode
(add-hook 'snippet-mode-hook
          '(lambda ()
             (set (make-local-variable 'require-final-newline) 1)))

;; org-mode
(add-to-list 'load-path (concat library-root "org-mode/lisp"))
(add-to-list 'load-path (concat library-root "org-mode/contrib/lisp"))
(add-hook 'org-load-hook
          '(lambda ()
             (add-to-list 'org-modules 'org-habit)
             (add-to-list 'org-modules 'org-mac-iCal)))

(defun camdez/org-agenda-mode-hook ()
  (interactive)
  (hl-line-mode 1)
  (setq show-trailing-whitespace nil))

(add-hook 'org-agenda-mode-hook
          'camdez/org-agenda-mode-hook)

(require 'org-install)
;(require 'ox-md)
(setq org-default-notes-file "~/org/notes.org"
      org-clock-idle-time 10
      org-deadline-warning-days 3
      org-enforce-todo-dependencies t
      org-agenda-restore-windows-after-quit t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-with-log-mode t
      org-agenda-span 'day
      org-agenda-archives-mode t
      org-log-done 'time
      org-agenda-custom-commands '(("h" "Habits"
                                    ((tags-todo "STYLE=\"habit\"")))
                                   ("n" "Agenda and all TODO's"
                                    ((agenda)
                                     (alltodo)))
                                   ("o" "Tasks older than a week"
                                    ((tags-todo "CREATED<=\"<-1w>\"")))
                                   ("f" "Agenda and Flagged Tasks"
                                    ((agenda "")
                                     (tags-todo "FLAGGED"))))
      org-capture-templates '(("t" "Task" entry (file+headline "~/org/personal.org" "Inbox")
                               "* TODO %?\n  %i")
                              ("p" "Post Topic for Blog" entry (file "~/org/posts.org")
                               "* %?\n  %i")
                              ("d" "Diary Entry" plain (file+datetree+prompt "~/org/diary.org")
                               "%?\n%i\n")
                              ("r" "Reading" entry (file+headline "~/org/reading.org" "In-Progress")
                               "* TODO _%?_ %t--\n")))

(setq org-mobile-inbox-for-pull "~/org/from-mobile.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; other
(add-to-list 'completion-ignored-extensions ".DS_Store") ; Never autocomplete .DS_Store files

;;; modes.el ends here
