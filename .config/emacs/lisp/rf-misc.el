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
;; https://www.emacswiki.org/emacs/AutoSave
;; http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/
(setq auto-save-default nil)

;; https://www.reddit.com/r/emacs/comments/29zm3q/comment/ciq24je/
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html#Interlocking
;; https://stackoverflow.com/questions/1629217/emacs-why-do-i-have-hashes-at-the-ends-of-my-file-names-i-e-test-c/72027075#72027075
;; https://stackoverflow.com/questions/5738170/why-does-emacs-create-temporary-symbolic-links-for-modified-files
;; https://emacs.stackexchange.com/questions/8193/untracked-file-in-a-git-repository
;; https://emacs.stackexchange.com/questions/55089/emacs-leaving-behind-files-preceded-with-despite-lock-files-being-disabled
;; https://www.emacswiki.org/emacs/LockFiles
(setq create-lockfiles nil)

;; https://emacs.stackexchange.com/questions/392/how-to-change-the-cursor-type-and-color/393#393
(setq-default cursor-type 'bar)

;; https://emacs.stackexchange.com/questions/24678/kill-the-current-line-and-preserving-cursor-position/49060#49060
;; https://stackoverflow.com/questions/637351/emacs-how-to-delete-text-without-kill-ring/65100416#65100416
;; https://unix.stackexchange.com/questions/26360/emacs-deleting-a-line-without-sending-it-to-the-kill-ring/136581#136581
;; https://www.emacswiki.org/emacs/BackwardKillLine
;; https://emacs.stackexchange.com/questions/12701/kill-a-line-deletes-the-line-but-leaves-a-blank-newline-character/12702#12702
(defun rf/kill-whole-line ()
  "Kill whole line but retain cursor position instead of moving to start of next line."
  (interactive)
  (let ((n (current-column)))
    (kill-whole-line)
    ;; do not move-to-column if that will add whitespaces at the end of line
    ;; https://emacs.stackexchange.com/questions/13068/is-there-a-function-that-returns-the-position-of-the-first-and-last-non-whitespa
    (if (< n (- (line-end-position) (line-beginning-position)))
      (move-to-column n t)
      (end-of-line))))

(defun rf/kill-line-or-region ()
  "If no region is selected kill the current line otherwise kill region."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end) t)
    (rf/kill-whole-line)))

(global-set-key (kbd "C-S-k") 'rf/kill-line-or-region)

;; https://emacs.stackexchange.com/questions/20896/change-the-behaviour-of-ret-with-electric-indent-to-only-indent-the-new-line/20899#20899
;; https://www.emacswiki.org/emacs/IndentationBasics
;; Emacs does enable the global electric-indent-mode by default
;; https://www.reddit.com/r/emacs/comments/g0v57a/comment/fnd5lg4/
(setq-default electric-indent-inhibit t)


;; https://emacs.stackexchange.com/questions/29973/stop-javascript-mode-from-lining-up-function-parameters-after-newline/34534#34534
(add-hook 'js-mode-hook
  (lambda ()
    (setq
      ;; https://emacs.stackexchange.com/questions/72678/switch-casess-code-block-doesnt-get-indented-in-javascript-mode/73072#73072
      ;; https://emacs.stackexchange.com/questions/54255/customize-js2-mode-switch-indentation
      ;; https://stackoverflow.com/questions/4177929/how-to-change-the-indentation-width-in-emacs-javascript-mode/4178127#4178127
      js-switch-indent-offset js-indent-level
      ;; when pressing RET in `console.log('Example',` indent only once
      ;; https://emacs.stackexchange.com/questions/29780changing-how-argument-lists-are-indented-in-javascript/34030#34030
      ;; https://emacs.stackexchange.com/questions/29973stop-javascript-mode-from-lining-up-function-parameters-after-newline/34534#34534
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

;; https://emacs.stackexchange.com/questions/42104/create-a-new-file-in-a-new-folder/42111#42111
;; https://www.iqbalansari.me/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun rofrol/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'rofrol/create-non-existent-directory)

;; https://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs/12512671#12512671
;; move the line(s) spanned by the active region up/down (line transposing)
(defun rofrol/move-lines (n)
  (let ((beg) (end) (keep))
    (if mark-active
        (save-excursion
          (setq keep t)
          (setq beg (region-beginning)
                end (region-end))
          (goto-char beg)
          (setq beg (line-beginning-position))
          (goto-char end)
          (setq end (line-beginning-position 2)))
      (setq beg (line-beginning-position)
            end (line-beginning-position 2)))
    (let ((offset (if (and (mark t)
                           (and (>= (mark t) beg)
                                (< (mark t) end)))
                      (- (point) (mark t))))
          (rewind (- end (point))))
      (goto-char (if (< n 0) beg end))
      (forward-line n)
      (insert (delete-and-extract-region beg end))
      (backward-char rewind)
      (if offset (set-mark (- (point) offset))))
    (if keep
        (setq mark-active t
              deactivate-mark nil))))

(defun rofrol/move-lines-up (n)
  "move the line(s) spanned by the active region up by N lines."
  (interactive "*p")
  (rofrol/move-lines (- (or n 1))))

(defun rofrol/move-lines-down (n)
  "move the line(s) spanned by the active region down by N lines."
  (interactive "*p")
  (rofrol/move-lines (or n 1)))

(global-set-key (kbd "M-<up>") 'rofrol/move-lines-up)
(global-set-key (kbd "M-<down>") 'rofrol/move-lines-down)

;; disable text resize with ctrl+mouse wheel/touchpad scroll
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

;; Based on crux-duplicate-and-comment-current-line-or-region
(defun rofrol/comment-current-line-or-region (arg)
  "Comments the current line or region"
  (interactive "p")
  (pcase-let* ((`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)))

(global-set-key [(control /)] 'rofrol/comment-current-line-or-region)

(setq mac-command-modifier 'control)

(provide 'rf-misc)
