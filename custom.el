;;; custom.el --- a place for Emacs to store customization info

;; Copyright (C) 2010  Cameron Desautels
;; Author: Cameron Desautels <camdez@gmail.com>

;;; Commentary:

;; Finally figured out how to get this crap out of the main Emacs config.  Use the following:
;;   (setq custom-file "~/.emacs.d/custom.el")
;;   (load custom-file)

;;; Code:

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(icicle-reminder-prompt-flag 0)
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 190 t)
 '(default-frame-alist (quote ((tool-bar-lines . 0) (cursor-type . box) (vertical-scroll-bars . right) (internal-border-width . 0) (modeline . t) (fringe) (background-mode . dark) (menu-bar-lines . 0) (right-fringe . 11) (left-fringe . 3) (border-color . "black") (cursor-color . "Red") (mouse-color . "black") (background-color . "black") (foreground-color . "white") (font . "-apple-lucida grande-medium-r-normal--13-0-72-72-m-0-iso10646-1")))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(show-paren-match-face ((((class color)) (:background "black" :foreground "red"))))
 '(text-mode-default ((t (:inherit default :strike-through nil :underline nil :slant normal :weight normal :height 120 :width normal :family "Monaco"))) t))

;;; custom.el ends here
