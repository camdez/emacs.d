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

;; text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

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

;; rst-mode
(add-hook 'rst-mode-hook 'turn-on-visual-line-mode)

;; ruby-mode
(add-to-list 'auto-mode-alist '("\\(Gem\\|Rake\\)file\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))

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
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

(eval-after-load 'js
  '(setq js-indent-level 2))

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
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; html-mode
(add-to-list 'auto-mode-alist '("\\.blog\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml\\'" . html-mode))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map "\C-c\C-p" 'php-mode))

;; yaml-mode
(add-to-list 'load-path (concat library-root "yaml-mode/"))
(autoload 'yaml-mode "yaml-mode"
  "Major mode for editing files in the YAML data serialization format" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(eval-after-load 'yaml-mode
  '(define-key yaml-mode-map "\C-m" 'newline-and-indent))

;; snippet-mode
(add-hook 'snippet-mode-hook
          '(lambda ()
             (set (make-local-variable 'require-final-newline) 1)))

;; org-mode
(add-to-list 'load-path (concat library-root "org-mode/lisp"))
(add-to-list 'load-path (concat library-root "org-mode/contrib/lisp"))
(require 'org-install)

;; remember-mode
(org-remember-insinuate)
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; other
(add-to-list 'completion-ignored-extensions ".DS_Store") ; Never autocomplete .DS_Store files

;;; modes.el ends here
