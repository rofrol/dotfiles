;;; -*- lexical-binding: t; -*-

;; https://manueluberti.eu/emacs/2021/08/06/fido/
;; http://xahlee.info/emacs/emacs/emacs_icomplete_mode.html
(fido-vertical-mode 1)

;; https://stackoverflow.com/questions/2627289/how-to-replace-a-region-in-emacs-with-yank-buffer-contents
;; When Delete Selection mode is enabled, typed text replaces the selection
;; if the selection is active.  Otherwise, typed text is just inserted at
;; point regardless of any selection.
(delete-selection-mode 1)

;; https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/init.el#L34
(setq use-short-answers t)

;; recentf stuff
;; https://stackoverflow.com/questions/50417/how-do-i-get-list-of-recent-files-in-gnu-emacs/50422#50422
;; https://stackoverflow.com/questions/3527150/open-recent-in-emacs/3527488
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-auto-cleanup 'never)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; https://www.littlehart.net/atthekeyboard/2017/05/26/letting-emacs-into-your-grumpy-heart/
;; You can disable backups, but that's a bad idea for obvious reasons https://superuser.com/questions/236883/why-does-emacs-create-a-file-that-starts-with
(setq make-backup-files nil)
;; http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/
(setq auto-save-default nil)

;; https://emacs.stackexchange.com/questions/392/how-to-change-the-cursor-type-and-color/393#393
(setq-default cursor-type 'bar)

;; https://stackoverflow.com/questions/637351/emacs-how-to-delete-text-without-kill-ring/65100416#65100416
;; https://unix.stackexchange.com/questions/26360/emacs-deleting-a-line-without-sending-it-to-the-kill-ring/136581#136581
;; https://www.emacswiki.org/emacs/BackwardKillLine
;; https://emacs.stackexchange.com/questions/12701/kill-a-line-deletes-the-line-but-leaves-a-blank-newline-character/12702#12702
(defun rofrol/delete-whole-line ()
  (interactive)
  (let (kill-ring)
    (kill-whole-line)))
;; Same but long version
;;  (delete-region
;;   (point)
;;   (save-excursion (move-end-of-line 1) (point)))
;;  (delete-char 1))

(global-set-key [(control shift k)] 'rofrol/delete-whole-line)

(provide 'rf-misc)