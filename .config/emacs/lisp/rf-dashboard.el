;;; -*- lexical-binding: t; -*-

;; https://www.reddit.com/r/emacs/comments/8jaflq/tip_how_to_use_your_dashboard_properly/
(rc/require 'dashboard)
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(setq dashboard-projects-backend 'project-el)
(add-to-list 'dashboard-items '(projects . 5))
(setq dashboard-startup-banner 'logo)

(provide 'rf-dashboard)