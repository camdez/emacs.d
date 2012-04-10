;;; custom.el --- a place for Emacs to store customization info
;;; Author: Cameron Desautels <camdez@gmail.com>

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
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 190 t)
 '(icicle-reminder-prompt-flag 0)
 '(org-agenda-files (quote ("~/org/diary.org" "~/org/maxrvu.org" "~/org/personal.org")))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))

;;; custom.el ends here
