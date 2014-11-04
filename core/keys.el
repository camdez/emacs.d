;;; keys.el --- Emacs keybindings
;;; Author: Cameron Desautels <camdez@gmail.com>

(global-set-key [f2] 'goto-line)
(global-set-key [f3] (lambda ()
                       (interactive)
                       (switch-to-buffer (other-buffer))))
(global-set-key [f8] 'eshell)
(global-set-key [f9] 'speedbar)
; If running under X, have [f10] toggle display of menu-bar (and
; tool-bar) instead of running tmm-menubar.
(if window-system
    (global-set-key [f10] 'toggle-chrome))
(global-set-key [f11] 'compile)
(global-set-key [f12] 'recompile)
(global-set-key [del] 'delete-char)
(eval-after-load 'smex
  '(global-set-key (kbd "M-x") 'smex))
(global-set-key "\C-x0" 'delete-window-replacement)
(global-set-key "\C-x1" 'delete-other-windows-replacement)
(global-set-key "\C-x4k" 'kill-buffer-other-window)
(global-set-key (kbd "C-x C-p") 'camdez/show-buffer-file-name) ; shadows `mark-page`
(global-set-key (kbd "C-x n h") 'camdez/narrow-to-paragraph)
(global-set-key [C-right] 'next-buffer)
(global-set-key [C-left] 'previous-buffer)
(global-set-key [C-down] 'find-file-at-point)
(global-set-key [C-up] 'next-buffer-same-file-basename)
; This practially makes the previous line useless, but the differences
; should be investigated.
;(global-set-key "\M-`" 'ff-find-other-file)
(global-set-key "\M-`" 'other-frame)
(global-set-key "\M-o" 'occur)

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c TAB") 'camdez/toggle-tab-width)
(global-set-key (kbd "C-c P") 'camdez/toggle-show-paren-style)
(global-set-key (kbd "C-c z") 'toggle-frame-maximized)
(global-set-key (kbd "C-c .") 'camdez/touch)
(global-set-key "\C-cf" 'auto-fill-mode)
(global-set-key "\C-ch" 'global-hl-line-mode)
(global-set-key "\C-c#" 'comment-region)
(global-set-key "\M-gf" 'find-function)

(autoload 'idomenu "idomenu" nil t)
(global-set-key (kbd "M-g i") 'idomenu)

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "C-c b") 'browse-url)
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key "\C-cg" 'gist-list)
;(global-set-key "\C-cl" 'linum-mode)
(global-set-key "\C-cm" 'magit-status)
(global-set-key "\C-cs" (lambda ()
                          (interactive)
                          (switch-to-buffer "*scratch*")))
(global-set-key "\C-ct" 'toggle-truncate-lines)
(global-set-key "\C-cw" 'whitespace-mode)
(global-set-key (kbd "C-'") 'other-window)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

(define-key isearch-mode-map "\C-e" 'camdez/isearch-yank-identifier)

(when window-system
  (global-unset-key "\C-z"))

(windmove-default-keybindings)

(set-register ?e (cons 'file user-init-file))  ; quickly jump to init.el (".emacs") with C-x r j e
(set-register ?k (cons 'file "~/Dropbox/Source/dotfiles/dot-emacs.d/core/keys.el"))
(set-register ?x (cons 'file "~/Dropbox/Source/dotfiles/dot-emacs.d/core/experimental.el"))
(set-register ?s (cons 'file "~/.zshrc"))     ; quickly jump to .bashrc with C-x r j s
(set-register ?o (cons 'file "~/org/personal.org"))
(set-register ?p (cons 'file "~/org/posts.org"))

;; How cool would it be if global-set-key could add a permanent
;; binding?

;;; keys.el ends here
