;;; modes.el --- load modes and tweaks for various file types
;;; Author: Cameron Desautels <camdez@gmail.com>

;; open files in desirable modes based on file extensions
(add-to-list 'auto-mode-alist '("\\.uc\\'" . java-mode)) ; open UnrealScript files as java
(add-to-list 'auto-mode-alist '("\\.cs\\'" . java-mode))
(add-to-list 'auto-mode-alist '("mutt-.*-[0-9]+-[0-9]+-[0-9]+\\'" . post-mode))
(add-to-list 'auto-mode-alist '("\\.blog\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

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

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.\\(mdown\\|markdown\\|markdn\\|md\\)\\'" . markdown-mode))

(autoload 'haml-mode "haml-mode"
  "Major mode for editing HAML files" t)
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

;; Don't insert spaces when inserting parentheses in php-mode
(add-hook 'php-mode-hook '(lambda ()
                            (setq parens-require-spaces nil)))

;; Mode-specific keybindings
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

;;; modes.el ends here
