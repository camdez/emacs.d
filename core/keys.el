;;; keys.el --- Emacs keybindings
;;; Author: Cameron Desautels <camdez@gmail.com>

(mapc #'(lambda (binding)
          (let ((key (car binding))
                (command (cadr binding)))
            (global-set-key (kbd key) command)))
      '(("<del>"           delete-char)
        ("<f11>"           compile)
        ("<f12>"           recompile)
        ("<f2>"            goto-line)
        ("<f3>"            camdez/switch-to-other-buffer)
        ("<f8>"            eshell)
        ("<f9>"            speedbar)
        ("C-'"             other-window)
        ("C-<down>"        find-file-at-point)
        ("C-<left>"        previous-buffer)
        ("C-<right>"       next-buffer)
        ("C-<up>"          next-buffer-same-file-basename)
        ("C-M-<backspace>" backward-kill-sexp)
        ("C-c #"           comment-region)
        ("C-c ."           camdez/touch)
        ("C-c H"           highlight-symbol-mode) ; also want `highlight-symbol-nav-mode`...
        ("C-c P"           camdez/toggle-show-paren-style)
        ("C-c SPC"         ace-jump-mode)
        ("C-c TAB"         camdez/toggle-tab-width)
        ("C-c a"           org-agenda)
        ("C-c b"           browse-url)
        ("C-c c"           org-capture)
        ("C-c e"           eval-and-replace)
        ("C-c f"           auto-fill-mode)
        ("C-c g"           gist-list)
        ("C-c h"           global-hl-line-mode)
        ("C-c m"           magit-status)
        ("C-c p"           projectile-command-map)
        ("C-c s"           camdez/switch-to-scratch)
        ("C-c t"           toggle-truncate-lines)
        ("C-c w"           whitespace-mode)
        ("C-c x"           camdez/add-experiment)
        ("C-c z"           toggle-frame-maximized)
        ("C-x 0"           camdez/delete-window)
        ("C-x 1"           camdez/delete-other-windows)
        ("C-x 4 k"         camdez/kill-buffer-other-window)
        ("C-x C-p"         camdez/show-buffer-file-name) ; shadows `mark-page`
        ("C-x \\"          align-regexp)
        ("C-x k"           kill-this-buffer)
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

(windmove-default-keybindings)

(set-register ?e (cons 'file user-init-file))  ; quickly jump to init.el (".emacs") with C-x r j e
(set-register ?k (cons 'file "~/.emacs.d/core/keys.el"))
(set-register ?x (cons 'file camdez/experiments-file))
(set-register ?s (cons 'file "~/.zshrc"))     ; quickly jump to shell config with C-x r j s
(set-register ?o (cons 'file "~/org/personal.org"))
(set-register ?p (cons 'file "~/org/posts.org"))

;; How cool would it be if global-set-key could add a permanent
;; binding?

;;; keys.el ends here
