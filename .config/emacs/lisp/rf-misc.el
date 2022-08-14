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

;; https://emacs.stackexchange.com/questions/24678/kill-the-current-line-and-preserving-cursor-position/49060#49060
;; https://stackoverflow.com/questions/637351/emacs-how-to-delete-text-without-kill-ring/65100416#65100416
;; https://unix.stackexchange.com/questions/26360/emacs-deleting-a-line-without-sending-it-to-the-kill-ring/136581#136581
;; https://www.emacswiki.org/emacs/BackwardKillLine
;; https://emacs.stackexchange.com/questions/12701/kill-a-line-deletes-the-line-but-leaves-a-blank-newline-character/12702#12702
(defun sto/kill-whole-line ()
  "Kill whole line but retain cursor position instead of moving to start of next line."
  (interactive)
  (let ((n (current-column)))
    (kill-whole-line)
    (move-to-column n t)))

(defun sto/kill-line-or-region ()
  "If no region is selected kill the current line otherwise kill region."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end) t)
    (sto/kill-whole-line)))

(global-set-key (kbd "C-S-k") 'sto/kill-line-or-region)

;; https://emacs.stackexchange.com/questions/20896/change-the-behaviour-of-ret-with-electric-indent-to-only-indent-the-new-line/20899#20899
;; https://www.emacswiki.org/emacs/IndentationBasics
;; Emacs does enable the global electric-indent-mode by default
;; https://www.reddit.com/r/emacs/comments/g0v57a/comment/fnd5lg4/
(setq-default electric-indent-inhibit t)


;; https://emacs.stackexchange.com/questions/29973/stop-javascript-mode-from-lining-up-function-parameters-after-newline/34534#34534
(add-hook 'js-mode-hook
  (lambda ()
    (setq js-switch-indent-offset js-indent-level
          ;; when pressing RET in `console.log('Example',` indent only once
          ;; https://emacs.stackexchange.com/questions/29780/changing-how-argument-lists-are-indented-in-javascript/34030#34030
          ;; https://emacs.stackexchange.com/questions/29973/stop-javascript-mode-from-lining-up-function-parameters-after-newline/34534#34534
          js-indent-align-list-continuation nil)))

;; Prevents issue where you have to press backspace twice when
;; trying to remove the first character that fails a search
;; https://stackoverflow.com/questions/285660/automatically-wrapping-i-search/36707038#36707038
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;; when I do an incremental search, the cursor jumps to the first match
;; whether the first match is above or below the cursor
;; https://stackoverflow.com/questions/285660/automatically-wrapping-i-search
;; (setq isearch-wrap-pause 'no-ding) not for incremental search, only for repeated
;; https://stackoverflow.com/questions/72957940/setting-isearch-wrap-pause-in-emacs-28/73071338#73071338
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

(provide 'rf-misc)