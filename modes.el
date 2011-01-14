;;; modes.el --- load modes and tweaks for various file types
;;; Author: Cameron Desautels <camdez@gmail.com>

;; open files in desirable modes based on file extensions
(add-to-list 'auto-mode-alist '("\\.uc\\'" . java-mode)) ; open UnrealScript files as java
(add-to-list 'auto-mode-alist '("\\.cs\\'" . java-mode))
(add-to-list 'auto-mode-alist '("mutt-.*-[0-9]+-[0-9]+-[0-9]+\\'" . post-mode))

;; text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; magit-mode
(autoload 'magit-status "magit"
  "Mode for working with git." t)

;; full-ack
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

;; Automagically turn on eldoc mode for emacs-lisp buffers
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(add-hook 'rst-mode-hook 'visual-line-mode)

;; ruby-mode
(add-to-list 'auto-mode-alist '("\\(Gem\\|Rake\\)file\\'" . ruby-mode))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.\\(mdown\\|markdown\\|markdn\\|md\\)\\'" . markdown-mode))

(autoload 'haml-mode "haml-mode"
  "Major mode for editing HAML files" t)
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

(autoload 'sass-mode "sass-mode"
  "Major mode for editing SASS files" t)
(add-to-list 'auto-mode-alist '("\\.\\(sass\\|scss\\)\\'" . sass-mode))

;; javascript-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(setq js-indent-level 2)

;; css-mode
(setq css-indent-offset 2)

;; php-mode
(add-hook 'php-mode-hook
  '(lambda ()
     (setq parens-require-spaces nil))) ; Don't insert spaces when inserting parentheses

;; html-mode
(add-to-list 'auto-mode-alist '("\\.blog\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml\\'" . html-mode))

(setq html-mode-hook
  '(lambda ()
     (auto-fill-mode 1)
     (define-key html-mode-map "\C-c\C-p" 'php-mode)))

(setq php-mode-hook
  '(lambda ()
     (define-key php-mode-map "\C-c\C-p" 'html-mode)))

(add-hook 'dired-mode-hook
  '(lambda ()
     (define-key dired-mode-map "\r" 'dired-find-alternate-file)))

(add-to-list 'completion-ignored-extensions ".DS_Store") ; Never autocomplete .DS_Store files

;;; modes.el ends here
