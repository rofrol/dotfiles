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

;; https://github.com/bbatsov/crux/blob/20c07848049716a0e1aa2560e23b5f4149f2a74f/crux.el#L286
(global-set-key (kbd "M-J") (lambda () (interactive) (delete-indentation 1)))

;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs/5058752#5058752
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; https://stackoverflow.com/questions/637351/emacs-how-to-delete-text-without-kill-ring/65100416#65100416
;; https://unix.stackexchange.com/questions/26360/emacs-deleting-a-line-without-sending-it-to-the-kill-ring/136581#136581
;; https://www.emacswiki.org/emacs/BackwardKillLine
(defun delete-line ()
  (interactive)
  (let (kill-ring)
    (kill-line)))
;; Same but long version
;;  (delete-region
;;   (point)
;;   (save-excursion (move-end-of-line 1) (point)))
;;  (delete-char 1))

(global-set-key (kbd "C-k") 'delete-line)

(defun delete-whole-line ()
  (interactive)
  (let (kill-ring)
    (kill-whole-line)))
;; Same but long version
;;  (delete-region (line-beginning-position) (line-end-position))
;;  (delete-char 1))

;;(global-set-key (kbd "C-K") 'delete-whole-line)
(global-set-key [(control shift k)] 'delete-whole-line)

;; https://github.com/bbatsov/crux/blob/308f17d914e2cd79cbc809de66d02b03ceb82859/crux.el#L224
(defun delete-line-backwards ()
  "Delete line backwards and adjust the indentation."
  (interactive)
  (let (kill-ring)
    (kill-line 0)
    (indent-according-to-mode)))

(global-set-key (kbd "M-K") 'delete-line-backwards)

(defun mark-whole-line ()
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))

(defun kill-ring-save-whole-line-or-region ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-ring-save) ;; then
    (save-mark-and-excursion ;; else
      (mark-whole-line)
      (kill-ring-save (region-beginning) (region-end))
      (pop-mark)
      )))

(define-key global-map (kbd "M-w") #'kill-ring-save-whole-line-or-region)
