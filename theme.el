;;; theme.el --- make things look pretty
;;; Author: Cameron Desautels <camdez@gmail.com>

(set-foreground-color "white")
(set-background-color "black")

(eval-after-load 'paren
  '(progn
     (set-face-foreground 'show-paren-match-face "red")
     (set-face-background 'show-paren-match-face "black")))

;; Aquamacs has issues with the basic set-b/f-color calls at init time
(when (featurep 'aquamacs)
  (setq default-frame-alist '((background-color . "black")
                              (foreground-color . "white"))))

;; Set font for Cocoa Emacs
(when (featurep 'ns)
  (setq ns-input-font "Droid Sans Mono")
  (setq ns-input-fontsize 13)
  (ns-respond-to-change-font))

;;; theme.el ends here
