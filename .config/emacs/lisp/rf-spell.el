;;; -*- lexical-binding: t; -*-

(defun my/flyspell-popup-below-word (args)
  "Keep keyboard-invoked Flyspell correction menus below point on a TTY."
  (pcase-let ((`(,event ,poss ,word) args))
    (when (and (null event) (not (display-graphic-p)))
      ;; Point may have moved in the same command; refresh its TTY coordinates.
      (redisplay)
      (let* ((position (popup-menu-normalize-position (point)))
             (xy (car-safe position))
             (window (cadr position))
             (corrections (nth 2 poss))
             ;; Title, optional separator, and the three dictionary actions.
             (menu-rows (+ (length corrections)
                           (if corrections 5 4))))
        (when (and (consp xy)
                   (integerp (cadr xy))
                   (windowp window)
                   (window-live-p window))
          ;; The TTY renderer moves a tall menu upward to keep its bottom
          ;; on-screen.  Move the buffer instead, just enough to leave the
          ;; complete menu below the word.
          (let ((target-row
                 (max 0 (- (window-body-height window) menu-rows 1))))
            (when (> (cadr xy) target-row)
              (with-selected-window window
                (recenter target-row)
                (redisplay))
              (setq position (popup-menu-normalize-position (point))
                    xy (car position))))
          ;; The renderer draws the menu title one row before its position.
          (setq event
                (list (list (car xy) (+ (cadr xy) 2))
                      (cadr position))))))
    (list event poss word)))

(with-eval-after-load 'flyspell
  (unless (advice-member-p #'my/flyspell-popup-below-word
                           'flyspell-emacs-popup)
    (advice-add 'flyspell-emacs-popup :filter-args
                #'my/flyspell-popup-below-word)))

(setq ispell-dictionary "pl_PL")
;; Emacs otherwise treats subtitle files as SRecode templates.
(add-to-list 'auto-mode-alist '("\\.srt\\'" . text-mode))


(defun my/text-mode-flyspell ()
  "Enable Flyspell with the Polish dictionary in text buffers."
  (setq-local ispell-local-dictionary "pl_PL")
  (flyspell-mode 1))

(add-hook 'text-mode-hook #'my/text-mode-flyspell)

(defun flyspell-goto-prev-error ()
  "Go to prev error."
  (interactive)
  (flyspell-goto-next-error t))
 
(defun my-flyspell-goto-next-and-correct ()
  "Go to next error and show correction menu"
  (interactive)
  (flyspell-goto-next-error)
  (flyspell-correct-word-before-point))
 
 (with-eval-after-load "flyspell"
   (define-key flyspell-mode-map (kbd "C-c '") 'flyspell-goto-next-error)
   (define-key flyspell-mode-map (kbd "C-c ;") 'flyspell-goto-prev-error)
   (define-key flyspell-mode-map (kbd "C-c \\") 'my-flyspell-goto-next-and-correct)
   (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-word-before-point))


(provide 'rf-spell)
