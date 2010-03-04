;;; keys.el --- Emacs keybindings
;;; Author: Cameron Desautels <camdez@gmail.com>

(global-set-key [f2] 'goto-line)
(global-set-key [f3] (lambda ()
                       (interactive)
                       (switch-to-buffer (other-buffer))))
(global-set-key [f8] 'eshell)
(global-set-key [f9] 'speedbar)
; If running under X, have [f10] toggle display of menu-bar (and
; tool-bar) instead of running tmm-menubar.  Don't toggle one off and
; one on.
(if window-system
    (global-set-key [f10] (lambda ()
                            (interactive)
                            (when (y-or-n-p "Are you sure you want to toggle the chrome? ")
                              (if (eq menu-bar-mode tool-bar-mode)
                                  (progn
                                    (menu-bar-mode nil)
                                    (tool-bar-mode))
                                (menu-bar-mode nil))))))
(global-set-key [f11] 'compile)
(global-set-key [f12] 'recompile)
(global-set-key [del] 'delete-char)
(global-set-key "\C-x0" 'delete-window-replacement)
(global-set-key "\C-x1" 'delete-other-windows-replacement)
(global-set-key "\C-x4k" 'kill-buffer-other-window)
(global-set-key [C-right] 'next-buffer)
(global-set-key [C-left] 'prev-buffer)
(global-set-key [C-up] 'next-buffer-same-file-basename)
; This practially makes the previous line useless, but the differences
; should be investigated.
(global-set-key "\M-`" 'ff-find-other-file)
(global-set-key "\M-o" 'occur)

(global-set-key "\C-cf" 'auto-fill-mode)
(global-set-key "\C-cl" 'goto-line)
(global-set-key "\C-ch" 'hl-line-mode)

(set-register ?e (cons 'file user-init-file))  ; quickly jump here with C-x r j e

;;; keys.el ends here