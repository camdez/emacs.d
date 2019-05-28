;;; theme.el --- make things look pretty
;;; Author: Cameron Desautels <camdez@gmail.com>

;;; Set up the basics
(set-foreground-color "white")
(set-background-color "black")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; use `current-frame-configuration' to check
(setq frame-inherited-parameters
      '(font fontsize background-color foreground-color))

;;; Load the main theme
(load-theme 'monokai t)

;;; Tweak additional faces
(set-face-background 'region "gray40")
(set-face-background 'trailing-whitespace "gray19")

(eval-after-load 'eldoc
  '(set-face-underline 'eldoc-highlight-function-argument "red"))

(eval-after-load 'paren
  '(progn
     (set-face-foreground 'show-paren-match "black")
     (set-face-background 'show-paren-match "red")
     (set-face-background 'show-paren-match-expression "gray35")
     (set-face-attribute 'show-paren-match-expression nil :inherit nil)))

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-removed "#F1266F")
     (set-face-foreground 'diff-added "#A6E22A")
     (set-face-foreground 'diff-changed "#66D9EF")))

;;; theme.el ends here
