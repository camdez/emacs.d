;;; keys.el --- Emacs keybindings
;;; Author: Cameron Desautels <camdez@gmail.com>

;;; Commentary:

;; Attempting to to put all bindings in one place so we can see the
;; lay of the land.

;;; Code:

(mapc #'(lambda (binding)
          (let ((key (car binding))
                (command (cadr binding)))
            (global-set-key (kbd key) command)))
      '(("<del>"           delete-char)
        ("<f2>"            goto-line)
        ("<f3>"            camdez/switch-to-other-buffer)
        ("<f8>"            eshell)
        ("<f9>"            speedbar)
        ("<f11>"           compile)
        ("<f12>"           recompile)
        ("C-'"             other-window)
        ("C-<down>"        find-file-at-point)
        ("C-<left>"        previous-buffer)
        ("C-<right>"       next-buffer)
        ("C-<up>"          next-buffer-same-file-basename)
        ("C-M-<backspace>" backward-kill-sexp)
        ("C-c #"           comment-region)
        ;; "C-c $"         toggle commenting line?  sexp?  See also "M-'" in experiments
        ("C-c ."           camdez/touch)
        ;; "C-c '"         camdez/change-outer (experimental)
        ;; "C-c ;"         mark-line (experimental)
        ;; "C-c B"         camdez/browse-url-dwim (experimental)
        ("C-c H"           highlight-symbol-mode) ; also want `highlight-symbol-nav-mode`...
        ("C-c N"           linum-mode)
        ("C-c P"           camdez/toggle-show-paren-style)
        ("C-c R"           rainbow-delimiters-mode)
        ("C-c SPC"         ace-jump-mode)
        ("C-c TAB"         camdez/toggle-tab-width)
        ("C-c a"           org-agenda)
        ("C-c b"           browse-url)
        ("C-c c"           org-capture)
        ;; "C-c d"         camdez/duplicate-paragraph (experimental) -- change to keychain to duplicate more things (lines, defuns, paragraphs, pages)
        ("C-c e"           eval-and-replace)
        ("C-c f"           auto-fill-mode)
        ("C-c g"           gist-list) ; FIXME: broken
        ("C-c h"           global-hl-line-mode)
        ;; "C-c i"         FREE
        ;; "C-c j"         ace (experimental)
        ;; "C-c k"         ace (experimental)
        ("C-c l"           org-store-link)
        ("C-c m"           magit-status)
        ;; "C-c n"         camdez/find-project-notes ; disabling while playing with Roam
        ;; "C-c o"         ...open stuff (experimental)
        ("C-c p"           projectile-command-map)
        ;; "C-c q"         FREE
        ;; "C-c r"         FREE
        ("C-c s"           camdez/switch-to-scratch)
        ("C-c t"           toggle-truncate-lines)
        ;; "C-c u"         FREE
        ;; "C-c v"         FREE
        ("C-c w"           whitespace-mode)
        ("C-c x"           camdez/add-experiment)
        ("C-c y"           helm-show-kill-ring)
        ("C-c z"           toggle-frame-maximized)
        ("C-c /"           isearch-forward-symbol-at-point) ; standard `M-s .` binding shadowed by Paredit
        ("C-x 0"           camdez/delete-window)
        ("C-x 1"           camdez/delete-other-windows)
        ("C-x 4 k"         camdez/kill-buffer-other-window)
        ("C-x C-p"         camdez/show-buffer-file-name) ; shadows `mark-page`
        ("C-x \\"          align-regexp)
        ("C-x k"           kill-this-buffer)
        ;; "C-x p"         back-window (experimental)
        ("C-x n h"         camdez/narrow-to-paragraph)
        ("M-Z"             zap-up-to-char)
        ("M-`"             other-frame)
        ("M-c"             camdez/capitalize-word)
        ("M-g f"           find-function)
        ("M-o"             occur)))

(when window-system
  (global-set-key (kbd "<f10>") 'camdez/toggle-chrome)
  (global-unset-key (kbd "C-z")))

(define-key isearch-mode-map (kbd "C-e") 'camdez/isearch-yank-identifier)

;; Actually quit, don't bury buffer.
(define-key Info-mode-map (kbd "q")
  #'(lambda ()
      (interactive)
      (quit-window t)))

(define-key special-mode-map (kbd "q")
  #'(lambda ()
      (interactive)
      (quit-window t)))

(set-register ?e (cons 'file user-init-file))  ; quickly jump to init.el (".emacs") with C-x r j e
(set-register ?k (cons 'file "~/.emacs.d/core/keys.el"))
(set-register ?x (cons 'file camdez/experiments-file))
(set-register ?s (cons 'file "~/.zshrc"))     ; quickly jump to shell config with C-x r j s
(set-register ?o (cons 'file "~/org/personal.org"))
(set-register ?p (cons 'file "~/org/posts.org"))

;;; keys.el ends here
