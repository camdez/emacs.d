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
            (dired-omit-mode 1)
            (define-key dired-mode-map "\r" 'dired-find-alternate-file)))

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

;; rst-mode
(add-hook 'rst-mode-hook 'visual-line-mode)

;; ruby-mode
(add-to-list 'auto-mode-alist '("\\(Gem\\|Rake\\)file\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.\\(mdown\\|markdown\\|markdn\\|md\\)\\'" . markdown-mode))
(add-hook 'markdown-mode-hook
  '(lambda ()
     (define-key markdown-mode-map (kbd "<tab>") 'yas/expand)))

;; haml-mode
(autoload 'haml-mode "haml-mode"
  "Major mode for editing HAML files" t)
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

;; sass-mode
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

;; php-mode
(setq php-mode-hook
  '(lambda ()
     (define-key php-mode-map "\C-c\C-p" 'html-mode)))

;; yaml-mode
(autoload 'yaml-mode "yaml-mode"
  "Major mode for editing files in the YAML data serialization format" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; snippet-mode
(add-hook 'snippet-mode-hook
          '(lambda ()
             (make-local-variable 'require-final-newline)
             (setq require-final-newline nil)))

;; other
(add-to-list 'completion-ignored-extensions ".DS_Store") ; Never autocomplete .DS_Store files

;;; modes.el ends here
