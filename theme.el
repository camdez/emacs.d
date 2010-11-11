;;; theme.el --- make things look pretty
;;; Author: Cameron Desautels <camdez@gmail.com>

(set-foreground-color "white")
(set-background-color "black")

;; Set font for Cocoa Emacs
(when (featurep 'ns)
  (setq ns-input-font "Droid Sans Mono")
  (setq ns-input-fontsize 13)
  (ns-respond-to-change-font))

; use `current-frame-configuration` to check
(setq frame-inherited-parameters
      '(font fontsize background-color foreground-color))

(eval-after-load 'paren
  '(progn
     (set-face-foreground 'show-paren-match-face "red")
     (set-face-background 'show-paren-match-face "black")))

;;; theme.el ends here
