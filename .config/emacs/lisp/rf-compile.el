;;; -*- lexical-binding: t; -*-

;; https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close
;; https://www.reddit.com/r/emacs/comments/44jwh3/comment/czte0lx/
(defun rf/delete-compile-window-if-successful (buffer string)
 "Delete a compilation window if succeeded without warnings "
 (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-at-time 3 nil
      (lambda ()
        (select-window (get-buffer-window (get-buffer-create "*compilation*")))
        (delete-window)))))
(add-hook 'compilation-finish-functions 'rf/delete-compile-window-if-successful)

; https://stackoverflow.com/questions/4657142/how-do-i-encourage-emacs-to-follow-the-compilation-buffer/12838221#12838221
(setq compilation-scroll-output 'first-error)

; https://stackoverflow.com/questions/9725015/how-do-i-make-the-compilation-window-in-emacs-to-always-be-a-certain-size/9726633#9726633
(setq compilation-window-height 12)

; https://www.masteringemacs.org/article/compiling-running-scripts-emacs
; communicate with the background process, same with C-u M-x compile
; compilator sometimes asks certain questions on the terminal (yes/no, etc etc
; https://stackoverflow.com/questions/3217408/interactive-compilation-mode-in-emacs-what-if-compiler-asks-a-question
(defadvice compile (before ad-compile-smart activate)
  "Advises `compile' so it sets the argument COMINT to t."
  (ad-set-arg 1 t))

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

; https://www.reddit.com/r/emacs/comments/1h2o8hr/comment/lzl73w5/
(defun rf/finish-focus-comp (&optional buf-or-proc arg2)
    (let* ((comp-buf (if (processp buf-or-proc)
                         (process-buffer buf-or-proc)
                       buf-or-proc))
           (window (get-buffer-window comp-buf)))
      (if window
          (select-window window)
        (switch-to-buffer-other-window comp-buf))))

(add-hook 'compilation-finish-functions 'rf/finish-focus-comp)

(provide 'rf-compile)
