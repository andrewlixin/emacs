(setq user-init-file (expand-file-name "init.el" (expand-file-name ".xemacs" "~")))

(load-file user-init-file)

(org-agenda-list)
