;; -*- mode: elisp -*-

(setq load-prefer-newer t)

;; Modularization based on
;; https://github.com/tonini/emacs.d
;; https://www.reddit.com/r/emacs/comments/3q50do/best_way_organization_config_files_in_the_emacs/
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name/10102154#10102154
;; There is also https://github.com/zenspider/package
;; https://github.com/bbatsov/prelude/blob/f9fb902185e1f7afabe281a25f5787e69ea7b6c9/core/prelude-core.el#L111
(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(require 'prelude-packages)

(setq url-http-attempt-keepalives nil)

;; Disable the splash screen (to enable it again, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transientk mode
(transient-mark-mode 1)

;; https://stackoverflow.com/questions/2627289/how-to-replace-a-region-in-emacs-with-yank-buffer-contents
(delete-selection-mode 1)

(set-frame-font "Ubuntu Mono-14" nil t)
;; https://stackoverflow.com/questions/26437034/emacs-line-height/26442029#26442029
(setq-default line-spacing 14)

;; https://www.emacswiki.org/emacs/CuaMode
;; https://stackoverflow.com/questions/2097890/enabling-control-c-and-control-v-copy-and-paste-in-emacs/2097950#2097950
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; https://www.reddit.com/r/emacs/comments/6iqtze/org_mreturn_annoyance/dj9l379/?utm_source=reddit&utm_medium=web2x&context=3
(setq org-M-RET-may-split-line '((item . nil)))

;; https://www.reddit.com/r/emacs/comments/o33r6z/how_do_i_swap_mreturn_and_return_in_orgmode/h2cejp5/
(add-hook 'org-mode-hook (lambda () (org-autolist-mode)))

;; https://emacs.stackexchange.com/questions/47097/unbind-cua-c-return-key-only-in-dired-mode/47106#47106
(defun special-c-return ()
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-insert-heading-respect-content)
    (cua-set-rectangle-mark))
  )

(define-key cua-global-keymap [(control return)] 'special-c-return)

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
;; ;; https://stackoverflow.com/questions/3695775/get-emacs-to-join-lines-when-killing-lines/3740251#3740251
(defun rofrol/join-lines ()
  "Join next line but keep indent on white-space only line."
  (interactive)
  (move-end-of-line nil)
  (delete-char 1)
  (while  (looking-at "[[:blank:]]")
    (delete-char 1))
  (insert " ")
  (left-char nil))
 
(global-set-key [(meta shift j)] 'rofrol/join-lines)

;; https://stackoverflow.com/questions/3695775/get-emacs-to-join-lines-when-killing-lines/3740251#3740251
(defun pull-line () "Pull the next line that contains anything up to the end of this one"
  (interactive)
  (save-excursion
    (end-of-line)
    (while (looking-at "[ \n\r\t]")
      (delete-char 1))
    (if (looking-back "^[[:blank:]]*[[:punct:][:alnum:]].*")
 (fixup-whitespace)
      (indent-according-to-mode))))

;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs/5058752#5058752
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; https://stackoverflow.com/questions/637351/emacs-how-to-delete-text-without-kill-ring/65100416#65100416
;; https://unix.stackexchange.com/questions/26360/emacs-deleting-a-line-without-sending-it-to-the-kill-ring/136581#136581
;; https://www.emacswiki.org/emacs/BackwardKillLine
(defun rofrol/delete-line ()
  (interactive)
  (let (kill-ring)
    (kill-line)))
;; Same but long version
;;  (delete-region
;;   (point)
;;   (save-excursion (move-end-of-line 1) (point)))
;;  (delete-char 1))

(global-set-key [(meta shift k)] 'rofrol/delete-line)

;; Based on https://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs/12512671#12512671
(defun rofrol/delete-whole-line-or-in-region-whole-lines ()
  (interactive)
  (let ((beg) (end)))
    (if mark-active
        (progn
	 (setq beg (region-beginning)
                end (region-end))
          (goto-char beg)
          (setq beg (line-beginning-position))
          (goto-char end)
          (setq end (line-beginning-position 2)))
      (setq beg (line-beginning-position)
            end (line-beginning-position 2)))
    (delete-region beg end))

(global-set-key [(control shift k)] 'rofrol/delete-whole-line-or-in-region-whole-lines)

;; https://github.com/bbatsov/crux/blob/308f17d914e2cd79cbc809de66d02b03ceb82859/crux.el#L224
(defun rofrol/delete-line-backwards ()
  "Delete line backwards and adjust the indentation."
  (interactive)
  (let (kill-ring)
    (kill-line 0)
    (indent-according-to-mode)))

(global-set-key [(meta shift k)] 'rofrol/delete-line-backwards)
		     
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

(global-set-key [(meta up)] 'rofrol/move-lines-up)
(global-set-key [(meta down)] 'rofrol/move-lines-down)

;; https://stackoverflow.com/questions/2249955/emacs-shift-tab-to-left-shift-the-block/35183657#35183657
;; https://stackoverflow.com/questions/11623189/how-to-bind-keys-to-indent-unindent-region-in-emacs
(defun rofrol/indent-region (numSpaces)
    (progn 
        ; default to start and end of current line
        (setq regionStart (line-beginning-position))
        (setq regionEnd (line-end-position))

        ; if there's a selection, use that instead of the current line
        (when (use-region-p)
            (setq regionStart (region-beginning))
            (setq regionEnd (region-end))
        )

        (save-excursion ; restore the position afterwards 
            (goto-char regionStart) ; go to the start of region
            (setq start (line-beginning-position)) ; save the start of the line
            (goto-char regionEnd) ; go to the end of region
            (setq end (line-end-position)) ; save the end of the line

            (indent-rigidly start end numSpaces) ; indent between start and end
            (setq deactivate-mark nil) ; restore the selected region
        )
    )
)

(defun rofrol/indent-lines (&optional N)
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    (* (or N 1) tab-width)))

(defun rofrol/untab-region (&optional N)
    (interactive "p")
    (rofrol/indent-region (* (* (or N 1) tab-width)-1)))

(defun  rofrol/tab-region (N)
    (interactive "p")
    (if (use-region-p)
        (rofrol/indent-region (* (or N 1) tab-width)) ; region was selected, call indent-region
        (rofrol/indent-lines N); else insert spaces as expected
    ))

(global-set-key [(control >)] 'rofrol/tab-region)
(global-set-key [(control <)] 'rofrol/untab-region)

;; https://stackoverflow.com/questions/3527142/how-do-you-redo-changes-after-undo-with-emacs/60163018#60163018
(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'undo-redo)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; scrolling
;; https://www.reddit.com/r/emacs/comments/p0bfs6/touchpad_vertical_scroll_gesture_scrolls_too_much/
;; https://emacs.stackexchange.com/questions/68014/touchpad-vertical-scroll-gesture-scrolls-too-much-lines-in-gui-on-windows-10
(setq mouse-wheel-progressive-speed nil)
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))

(global-set-key [(control f)] 'isearch-forward)
(global-set-key [(control s)] 'save-buffer)

;; https://emacs.stackexchange.com/questions/3458/how-to-switch-between-windows-quickly
(global-set-key [(meta p)] 'ace-window)

;; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up/3008#3008
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(global-unset-key (kbd "C-x s"))
(global-set-key [(f3)] 'isearch-repeat-forward)
(global-set-key [(shift f3)] 'isearch-repeat-backward)

(defun rofrol/zen-mode ()
  (interactive)
  (tool-bar-mode -1)
  (toggle-frame-fullscreen))

;; https://www.emacswiki.org/emacs/FullScreen
;; https://emacs.stackexchange.com/questions/29624/how-do-i-make-sure-a-frame-is-fullscreen#comment45524_29624
(defun rofrol/zen-mode ()
  "Toggles full-screen mode and bars."
  (interactive)
  (if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
    (progn
      (menu-bar-mode t)
      (tool-bar-mode t))
    (progn
      (menu-bar-mode -1)
      (tool-bar-mode -1)))
  (toggle-frame-fullscreen))

(global-set-key [f11] 'rofrol/zen-mode)

;; https://www.emacswiki.org/emacs/ShowParenMode
(show-paren-mode 1)
(setq show-paren-delay 0)

;; show border around zero width space `​` https://news.ycombinator.com/item?id=16754256
;; to insert: `C-x 8 RET` then search for ZERO WIDTH SPACE
;; or `C-q 20013 RET'
(update-glyphless-char-display 'glyphless-char-display-control '((format-control . empty-box) (no-font . hex-code)))

;; https://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs#comment90221710_998472
(global-set-key [(meta shift down)] 'crux-duplicate-current-line-or-region)

;; Based on crux-duplicate-and-comment-current-line-or-region
(defun rofrol/comment-current-line-or-region (arg)
  "Comments the current line or region"
  (interactive "p")
  (pcase-let* ((`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)))

(global-set-key [(control /)] 'rofrol/comment-current-line-or-region)
(global-set-key [(meta control /)] 'crux-duplicate-and-comment-current-line-or-region)

;; https://superuser.com/questions/354849/emacs-kill-buffer-without-prompt/354878#354878
(global-set-key [(control w)] 'kill-this-buffer)

;; https://emacs.stackexchange.com/questions/392/how-to-change-the-cursor-type-and-color
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Cursor-Display.html
;; (setq-default cursor-type 'bar)
(setq-default cursor-type '(bar . 2))
;; (setq-default cursor-type 'hollow)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
