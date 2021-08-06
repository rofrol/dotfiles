;; -*- mode: elisp -*-

;; Disable the splash screen (to enable it again, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; https://emacs.stackexchange.com/questions/3171/function-to-open-my-init-file-for-editing-or-return-its-path
(global-set-key (kbd "<f8>") (lambda () (interactive) (find-file user-init-file)))
;; https://stackoverflow.com/questions/189490/where-can-i-find-my-emacs-file-for-emacs-running-on-windows
;; https://stackoverflow.com/questions/2580650/how-can-i-reload-emacs-after-changing-it/51781491#51781491
(global-set-key (kbd "<f9>") (lambda () (interactive) (load user-init-file)))

;; recentf stuff
;; https://stackoverflow.com/questions/50417/how-do-i-get-list-of-recent-files-in-gnu-emacs/50422#50422
;; https://stackoverflow.com/questions/3527150/open-recent-in-emacs/3527488
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-auto-cleanup 'never)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; https://stackoverflow.com/questions/1072662/by-emacs-how-to-join-two-lines-into-one/68685485#68685485
(defun join-lines ()
  (interactive)
  (next-line)
  (join-line)
  (delete-horizontal-space)
  (unless (looking-at-p "\n") (insert " ")))

(global-set-key (kbd "M-J") 'join-lines)

;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs/5058752#5058752
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
