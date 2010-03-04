;;; skeletons.el --- templates, mostly for new files
;;; Author: Cameron Desautels <camdez@gmail.com>

(define-skeleton blog-entry-skeleton
  "Inserts a PyBlosxom-style blog entry skeleton into the current buffer.
This only makes sense for empty buffers."
  "Title: "
  str | "Untitled Entry" \n
  "#postdate " (insert-date-and-time) \n
  "#tags " ("Enter a tag: " str ",") '(delete-backward-char 1) \n \n
  "<p>" _ "</p>")

(define-skeleton c-c++-header-skeleton
  "Inserts a skeleton for C and C++ header files in the format I like."
  (upcase
   (concat
    "__"
    (replace-regexp-in-string "\\." "_" (upcase (file-name-nondirectory buffer-file-name)))
    "__"))
  "#ifndef " str n "#define " str "\n\n" _ "\n\n#endif /*" str "*/")

(define-skeleton php-header-skeleton
  "Inserts a small PHP skeleton."
  "Description: "
  "<?php" \n \n
  "// " (file-name-nondirectory buffer-file-name) " - " str \n
  "//" \n
  "// Author: " user-full-name " <" user-mail-address ">" \n
  "// Date:   " (insert-date-and-time) \n
  "//" \n
  (copyright user-copyright-holder) \n \n
  _ \n \n
  "?>" \n
  )

(require 'autoinsert)

;; Autoinsert text into empty files where defined
(add-hook 'find-file-hooks 'auto-insert)

;; TODO rewrite this -- CARs can be mode-names instead of regexps, which is generally cleaner
(eval-after-load "autoinsert"
  '(progn
     (add-to-list 'auto-insert-alist '("\\.blog\\'" . blog-entry-skeleton))
     (add-to-list 'auto-insert-alist '("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . c-c++-header-skeleton))
     (add-to-list 'auto-insert-alist '("\\.php\\'" . php-header-skeleton))))

;;; skeletons.el ends here
