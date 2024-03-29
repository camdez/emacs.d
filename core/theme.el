;;; theme.el --- make things look pretty  -*- lexical-binding: t; -*-
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

(set-face-foreground 'fringe "grey30")
(set-face-background 'region "gray40")
(set-face-background 'trailing-whitespace "gray19")

(when (and window-system
	         (x-list-fonts "Fira Code"))
  (set-face-attribute 'font-lock-comment-face nil
                      :font "Fira Code"
                      ;; Must be explicitly unspecified, or won't
                      ;; scale with `:text-scale-adjust'.  Specifying
                      ;; probably works too, but leaving it out makes
                      ;; a mess.
                      :height 'unspecified
                      :slant 'italic))

(eval-after-load 'org-faces
  '(when (and window-system
              (x-list-fonts "Helvetica"))
     (set-face-attribute 'org-document-title nil
                         :font "Helvetica"
                         :height 2.5
                         :weight 'bold)))

(eval-after-load 'org-faces
  '(set-face-attribute 'org-clock-overlay nil
                       :background 'unspecified
                       :foreground "#E6DB74"
                       :box "#E6DB74"))

;; Make active window more obvious.
;;
;; Still deciding if I like this box...
(let ((box t))
  (set-face-attribute 'mode-line nil
                      :background "#49483E"
                      :foreground "white"
                      :box (when box "grey35"))

  (set-face-attribute 'mode-line-inactive nil
                      :background "gray10"
                      :box (when box "#75715E")))

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
