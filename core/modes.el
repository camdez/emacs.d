;;; modes.el --- load modes and tweaks for various file types
;;; Author: Cameron Desautels <camdez@gmail.com>

;; prog-mode
(defun camdez/prog-mode-hook ()
  (interactive)
  (setq show-trailing-whitespace t)
  (highlight-symbol-mode 1))

(add-hook 'prog-mode-hook 'camdez/prog-mode-hook)

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
  '(progn
     (define-key help-mode-map "l" 'help-go-back) ; consistent with info-mode
     (define-key help-mode-map "i" 'camdez/Info-goto-from-command-help)))

;; text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'html-mode-hook 'turn-off-auto-fill) ; inherits from text-mode

;; sh-mode
(defun camdez/shell-execute-line ()
  "Execute the current line as a shell command."
  (interactive)
  (sh-execute-region (line-beginning-position) (line-end-position)))

(eval-after-load 'sh-script
  '(progn
     (setq sh-basic-offset 2)
     (define-key sh-mode-map (kbd "C-c C-e") 'camdez/shell-execute-line)))

;; cc-mode
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 2)))

;; magit-mode
(autoload 'magit-status "magit"
  "Mode for working with git." t)

(when (require 'ack-and-a-half nil 'no-error)
  (defalias 'ack 'ack-and-a-half)
  (defalias 'ack-same 'ack-and-a-half-same)
  (defalias 'ack-find-file 'ack-and-a-half-find-file)
  (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
  (define-key ack-and-a-half-mode-map (kbd "n") 'next-error-no-select)
  (define-key ack-and-a-half-mode-map (kbd "p") 'previous-error-no-select))

;; occur-mode
;; `occur-mode' uses M-n, M-p, but ack and `grep-mode' support n and
;; p. `grep-mode' even supports TAB and <backtab>.  (Though there's
;; actually a bit of difference in functionality between the M / non-M
;; versions.)
(define-key occur-mode-map "n" 'occur-next)
(define-key occur-mode-map "p" 'occur-prev)

;; slime - the superior lisp interaction mode for emacs
(when (require 'slime nil t)
  (slime-setup))

(setq inferior-lisp-program "sbcl --noinform --no-linedit")

;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)

;; shell-mode
(add-hook 'shell-mode-hook (lambda ()
                             (setq tab-width 8)))

;; rst-mode
(add-hook 'rst-mode-hook 'turn-on-visual-line-mode)

;; ruby-mode
(let ((mode-files '("Capfile" "Gemfile" "Guardfile" "Podfile" "Rakefile" "config.ru"))
      (mode-extns '("rake" "gemspec" "jbuilder" "builder" "prawn")))
  (add-to-list 'auto-mode-alist (cons (concat       (regexp-opt mode-files) "\\'") 'ruby-mode))
  (add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt mode-extns) "\\'") 'ruby-mode)))

;; TODO consider precisely what these should be and then submit a
;; patch upstream.
(defun camdez/ruby-fix-paragraph-delimiters ()
  "Change the `ruby-mode' paragraph delimiters so paragraph
movement is not thwarted by trailing whitespace.  Also work for
paragraphs in comment blocks."
  (interactive)
  (setq paragraph-start (concat "[ 	]*#*[ 	]*$\\|" page-delimiter)
        paragraph-separate paragraph-start))

(defun camdez/ruby-replace-symbols ()
  "Interactively replace old style Ruby hash syntax with 1.9
syntax."
  (interactive)
  (query-replace-regexp "\\_<:\\([a-z_]+\\>\\)\\(\\s-*\\)=>" "\\1:\\2"))

(add-hook 'ruby-mode-hook 'camdez/ruby-fix-paragraph-delimiters)

(add-hook 'ruby-mode-hook
          '(lambda ()
             (define-key ruby-mode-map "\C-x\C-t" 'transpose-lines)
             (define-key ruby-mode-map (kbd "C-c :") 'camdez/ruby-replace-symbols)
             (define-key ruby-mode-map (kbd "C-c C-d") 'yari)))

(add-hook 'ruby-mode-hook 'imenu-add-menubar-index)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.\\(mdown\\|markdown\\|markdn\\|md\\)\\'" . markdown-mode))

(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map (kbd "<tab>") 'yas-expand))

(defvar camdez/markdown-imenu-generic-expression
  '((nil "^#\\s-+\\(.+\\)$" 1)))

(defun camdez/markdown-imenu-configure ()
  (interactive)
  (setq imenu-generic-expression camdez/markdown-imenu-generic-expression))

(defun camdez/marked-open-current-file ()
  "Open a preview of the current (presumably Markdown) file in
Marked.app."
  (interactive)
  ;; JEG2 uses this:
  ;;(call-process "open" nil nil nil "-a" "Marked.app" (buffer-file-name))
  (shell-command (format "open -a \"Marked 2.app\" %s"
                         (shell-quote-argument (buffer-file-name)))))

(add-hook 'markdown-mode-hook 'camdez/markdown-imenu-configure)
(add-hook 'markdown-mode-hook 'imenu-add-menubar-index)

;; TODO Test for existence of the app file before taking over binding.
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map (kbd "C-c C-c p") 'camdez/marked-open-current-file)) ; shadows `markdown-preview'

;; haml-mode
(autoload 'haml-mode "haml-mode"
  "Major mode for editing HAML files" t)
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

;; sass-mode
(autoload 'sass-mode "sass-mode"
  "Major mode for editing SASS files" t)
(add-to-list 'auto-mode-alist '("\\.\\(sass\\|scss\\)\\'" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.sass.erb\\'" . sass-mode))

;; css-mode
(eval-after-load 'css-mode
  '(setq css-indent-offset 2))

;; js-mode
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?\\'" . js-mode))

(eval-after-load 'js
  '(setq js-indent-level 2))

;; coffee-mode
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
(autoload 'clojure-mode "clojure-mode"
  "Major mode for editing Clojure.")
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

(setq cljr-clojure-test-declaration "[clojure.test :refer :all]")

(add-hook 'clojure-mode-hook 'imenu-add-menubar-index)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'bug-reference-prog-mode)
(add-hook 'cider-mode-hook 'eldoc-mode)

(defun camdez/clojure-mode-hook ()
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook 'camdez/clojure-mode-hook)

(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (.addShutdownHook 'defun)
     (fact 'defun)
     (facts 'defun)
     (against-background 'defun)
     (provided 0)
     (component/system-map 'defun)))

;; html-mode
(add-to-list 'auto-mode-alist '("\\.blog\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml\\'" . html-mode))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map "\C-c\C-p" 'php-mode))

;; yaml-mode
(autoload 'yaml-mode "yaml-mode"
  "Major mode for editing files in the YAML data serialization format" t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(eval-after-load 'yaml-mode
  '(define-key yaml-mode-map "\C-m" 'newline-and-indent))

;; conf-mode
(add-to-list 'auto-mode-alist '("Procfile\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("Gemfile.lock\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.git\\(config\\|modules\\)\\'" . conf-mode))

;; snippet-mode
(add-hook 'snippet-mode-hook
          '(lambda ()
             (set (make-local-variable 'require-final-newline) 1)))

;; org-mode
(add-hook 'org-load-hook
          '(lambda ()
             (add-to-list 'org-modules 'org-habit)))

(defun camdez/org-agenda-mode-hook ()
  (interactive)
  (hl-line-mode 1)
  (setq show-trailing-whitespace nil))

(add-hook 'org-agenda-mode-hook
          'camdez/org-agenda-mode-hook)

(require 'org-install nil t)
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
                              ("n" "Note" entry (file)
                               "* %?\n  %i")
                              ("r" "Reading" entry (file+headline "~/org/reading.org" "In-Progress")
                               "* TODO _%?_ %t--\n")
                              ("w" "Work Log Entry" plain (file+datetree+prompt "~/org/worklog.org")
                               "%?\n%i\n"))
      org-mobile-inbox-for-pull "~/org/from-mobile.org"
      org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; other
(add-to-list 'completion-ignored-extensions ".DS_Store") ; Never autocomplete .DS_Store files

;;; modes.el ends here
