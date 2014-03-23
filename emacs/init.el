;; init.el
;; Bootstrap the Emacs environment to load literate Emacs initialization files.

; First, establish a root directory from which we can locate the org-mode files we need.
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

; Locate the directory that has the org-mode files
(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org-mode" (expand-file-name
                                dotfiles-dir)))))
  (add-to-list 'load-path org-dir)
  (require 'org)
  (require 'ob))

; Load all literate org-mode files in this directory (any org-mode files residing there)
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "readme.org$"))
