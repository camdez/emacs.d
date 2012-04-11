;;; theme.el --- make things look pretty
;;; Author: Cameron Desautels <camdez@gmail.com>

;; Set font for Cocoa Emacs
(when (featurep 'ns)
  (setq ns-input-font "Inconsolata")
  (setq ns-input-fontsize 14)
  (ns-respond-to-change-font))

(set-foreground-color "white")
(set-background-color "black")

; use `current-frame-configuration' to check
(setq frame-inherited-parameters
      '(font fontsize background-color foreground-color))

(eval-after-load 'paren
  '(progn
     (set-face-foreground 'show-paren-match-face "red")
     (set-face-background 'show-paren-match-face "black")))

(when window-system
  (add-to-list 'load-path (concat library-root "color-theme/"))
  (add-to-list 'load-path (concat library-root "color-theme/themes/"))
  (require 'color-theme)
  (require 'color-theme-almost-monokai)
  (eval-after-load "color-theme"
    '(progn
       (color-theme-initialize)
       (color-theme-almost-monokai))))

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-removed "#F1266F")
     (set-face-foreground 'diff-added "#A6E22A")
     (set-face-foreground 'diff-changed "#66D9EF")))

;;; theme.el ends here
